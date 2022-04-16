(ns chic.clj-editor.nav
  (:require
   [taoensso.encore :as enc]
   [chic.clj-editor.ast.string :as ast.string]
   [chic.clj-editor.ui :as clj-editor.ui]
   [io.github.humbleui.core :as hui]
   [clj-commons.primitive-math :as prim]
   [chic.util :as util :refer [deftemplate defgenerated let-macro-syms]]
   [chic.clj-editor.ast :as ast]))

(deftemplate $-next-seg-pos
  [$advance-seg-idx $advance-line-idx $init-seg-idx
   ast ref-pos]
  (let [line-id (:line ref-pos)
        seg-idx (:seg-idx ref-pos)
        lines (::ast/lines ast)]
    (if-some [idx2 (and seg-idx ($advance-seg-idx seg-idx (get lines line-id)))]
      {:line line-id :seg-idx idx2}
      (let [line-order (::ast/line-order ast)]
        (when-some [next-line-idx ($advance-line-idx (util/index-of line-order line-id)
                                                     line-order)]
          (let [next-line-id (nth line-order next-line-idx)
                init-seg-idx ($init-seg-idx (get lines next-line-id))]
            (cond-> {:line next-line-id}
              init-seg-idx
              (assoc :seg-idx init-seg-idx))))))))

(defgenerated next-seg-pos [ast ref-pos]
  $-next-seg-pos
  (fn [t]
    (util/sub-template-args
     t
     ['$advance-seg-idx :inline-fn
      (fn [seg-idx segs]
        `(let [idx2# (inc ~seg-idx)]
           (when (< idx2# (count ~segs)) idx2#)))]
     ['$advance-line-idx :inline-fn
      (fn [next-line-idx line-order]
        `(let [idx2# (inc ~next-line-idx)]
           (when (< idx2# (count ~line-order)) idx2#)))]
     ['$init-seg-idx :inline-fn
      (fn [segs]
        `(when (< 0 (count ~segs)) 0))])))

(defgenerated prev-seg-pos [ast ref-pos]
  $-next-seg-pos
  (fn [t]
    (util/sub-template-args
     t
     ['$advance-seg-idx :inline-fn
      (fn [seg-idx _segs]
        `(when (< 0 ~seg-idx) (dec ~seg-idx)))]
     ['$advance-line-idx :inline-fn
      (fn [next-line-idx _line-order]
        (let-macro-syms [next-line-idx next-line-idx]
          `(when (< 0 ~next-line-idx) (dec ~next-line-idx))))]
     ['$init-seg-idx :inline-fn
      (fn [segs]
        `(let [sc# (count ~segs)]
           (when (< 0 sc#) (dec sc#))))])))

(defn seg-rect [self line-id seg-idx]
  (when-some [lst ^java.util.ArrayList
              (.get ^java.util.HashMap (:line->segs self) line-id)]
    (:rect (.get lst seg-idx))))

(defn closest-seg-in-row [^java.util.ArrayList ui-segs xloc allow-fn dist-method]
  (let [calc-dist (case dist-method
                    :centre
                    (fn [rect xloc]
                      (abs (- xloc (+ (int (/ (:width rect) 2)) (:x rect)))))
                    :edge
                    (fn [{:keys [x] :as rect} xloc]
                      (if (<= xloc x)
                        (- x xloc)
                        (if (<= xloc (:right rect))
                          0
                          (- xloc (:right rect))))))]
    (loop [seg-i (unchecked-int 0)
          prev-i nil
          prev-dist (unchecked-int Integer/MAX_VALUE)]
     (if (< seg-i (.size ui-segs))
       (let [rect (:rect (.get ui-segs seg-i))
             distance (calc-dist rect xloc)]
         (if (allow-fn seg-i)
           (if (< prev-dist distance)
             prev-i
             (recur (unchecked-inc-int seg-i)
                    seg-i
                    distance))
           (recur (unchecked-inc-int seg-i)
                  prev-i
                  prev-dist)))
       prev-i))))

(defn upordown-seg-pos [upordown self ast ref-pos]
  (let [line-order (::ast/line-order ast)
        this-line-id (:line ref-pos)]
    (when-some [next-line-id (nth line-order
                                  ((case upordown :up dec :down inc)
                                   (util/index-of line-order this-line-id))
                                  nil)]
      (enc/merge
       {:line next-line-id
        :vertical-lock-x (:vertical-lock-x ref-pos)}
       (if-some [next-row-of-segs ^java.util.ArrayList
                   (.get ^java.util.HashMap (:line->segs self) next-line-id)]
         (let [nodes (::ast/nodes ast)
               next-line (get (::ast/lines ast) next-line-id)
               pos (or (when-some [xloc (or (:vertical-lock-x ref-pos)
                                            (when (:local-idx ref-pos)
                                              (:x (clj-editor.ui/request
                                                      self ::clj-editor.ui/get-cursor-rect nil)))
                                            (when-some [seg-idx (:seg-idx ref-pos)]
                                              (when-some [the-seg-rect (seg-rect self this-line-id seg-idx)]
                                                (unchecked-int (+ (:x the-seg-rect) (/ (:width the-seg-rect) 2))))))]
                         (let [seg-idx (closest-seg-in-row
                                        next-row-of-segs xloc
                                        (fn [i] (not (ast/whitespace?
                                                      (get nodes (ast/line-seg->node-id (get next-line i))))))
                                        (if (:local-idx ref-pos) :edge :centre))]
                           {:vertical-lock-x xloc
                            :seg-idx seg-idx
                            :local-idx (when (:local-idx ref-pos)
                                         (clj-editor.ui/request
                                          self [::clj-editor.ui/cursor-rect->idx.for-seg
                                                next-line-id seg-idx]
                                          {:rect {:x xloc}}))}))
                       (when (prim/< 0 (.size next-row-of-segs))
                         {:seg-idx (or (first
                                        (keep-indexed
                                         (fn [i seg]
                                           (when-not (ast/whitespace? (get nodes (ast/line-seg->node-id seg)))
                                             i))
                                         next-line))
                                       0)}))]
           pos)
         (when (:local-idx ref-pos)
           {:local-idx 0}))))))

(defn -pos-until-nonws [ast f pos]
  (let [lines (::ast/lines ast)
        nodes (::ast/nodes ast)]
    (loop [prev-pos pos]
      (let [pos (f prev-pos)
            line (get lines (:line pos))
            seg-idx (:seg-idx pos)]
        (if (and (some? seg-idx)
                 (ast/whitespace?
                  (get nodes (ast/line-seg->node-id
                              (get line seg-idx))))
                 (not= pos prev-pos))
          (recur pos)
          pos)))))

(def ^java.util.HashMap position-overrides-by-type (java.util.HashMap.))

(defn pos->seg [ast pos]
  (let [line (get (::ast/lines ast) (:line pos))
        seg-idx (:seg-idx pos)]
    (when (and line seg-idx)
      (nth line seg-idx))))

(defn seg->node [ast seg]
  (get (::ast/nodes ast) (ast/line-seg->node-id seg)))

(defn pos->node [ast pos]
  (seg->node ast (pos->seg ast pos)))

(defn seg->string [node seg]
  (case (nth seg 0)
    ::ast/seg.simple
    (ast.string/simple-node->string node)
    ::ast/seg.comp-start
    (ast.string/composite-node->string-start node)
    ::ast/seg.comp-end
    (ast.string/composite-node->string-end node)))

(defn assoc-pos-local-idx [pos idx]
  (assoc (dissoc pos :vertical-lock-x) :local-idx idx))

(defn resolve-pos [self ast ref-pos vpos]
  (let [seg (pos->seg ast ref-pos)
        node (seg->node ast seg)
        vpos (if-some [f (.get position-overrides-by-type (::ast/node-type node))]
               (f vpos ref-pos node)
               vpos)
        ref-idx (:local-idx ref-pos)
        r (if (map? vpos)
            vpos
            (case vpos
              :next
              (let [repr (when node (seg->string node seg))]
                (if (<= (count repr) ref-idx)
                  (resolve-pos self ast ref-pos :next-seg)
                  (assoc-pos-local-idx ref-pos (inc ref-idx))))
              :prev
              (if (<= ref-idx 0)
                (resolve-pos self ast ref-pos :prev-seg)
                (assoc-pos-local-idx ref-pos (dec ref-idx)))
              :next-seg
              (let [r (-pos-until-nonws ast #(next-seg-pos ast %) ref-pos)]
                (or (when (:local-idx ref-pos) (resolve-pos self ast r :insert-left)) r))
              :prev-seg
              (let [r (-pos-until-nonws ast #(prev-seg-pos ast %) ref-pos)]
                (or (when (:local-idx ref-pos) (resolve-pos self ast r :insert-right)) r))
              :down-seg
              (upordown-seg-pos :down self ast ref-pos)
              :up-seg
              (upordown-seg-pos :up self ast ref-pos)
              :end-insertion
              (prn "lol")
              :insert-left
              (assoc-pos-local-idx ref-pos 0)
              :insert-right
              (assoc-pos-local-idx
               ref-pos (count (when node (seg->string node seg))))
              (println "unknown vpos:" vpos)))]
    r))

(defn ensure-cursor-in-view [self ast pos]
  (let [line-id (:line pos)
        ;; cursor-rect (:rect (:ui-cursor self))
        ;; line-rect (:rect (get (:ui-lines self) line-id))
        ui-vscroll (:ui-vscroll self)
        rects (clj-editor.ui/request self ::clj-editor.ui/get-line-rects nil)
        ;; ui-column (:child ui-vscroll)
        line-order (::ast/line-order ast)
        line-idx (util/index-of line-order line-id)
        line-rect (.get ^java.util.ArrayList rects line-idx)
        view-rect (:size ui-vscroll)
        offset (:offset ui-vscroll)]
    (when line-rect
      (let [diff-below (- (:bottom line-rect) (:bottom view-rect))]
        (if (pos? diff-below)
          (hui/-set! ui-vscroll :offset (- offset diff-below))
          (let [diff-top (- (:y view-rect) (:y line-rect))]
            (when (pos? diff-top)
              (hui/-set! ui-vscroll :offset (+ offset diff-top)))))))))

(.put position-overrides-by-type
      ::ast/type.symbol
      (fn [vpos ref-pos node]
        (let [nstr (ast.string/simple-node->string node)]
          (case vpos
            :insert-right
            (assoc-pos-local-idx ref-pos (count nstr))
            :insert-left
            (assoc-pos-local-idx ref-pos 0)
            vpos))))
