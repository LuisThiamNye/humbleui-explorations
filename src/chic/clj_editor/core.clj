(ns chic.clj-editor.core
  (:require
   [clj-commons.primitive-math :as prim]
   [chic.clj-editor.seg :as clj-editor.seg]
   [chic.clj-editor.event :as clj-editor.event]
   [potemkin :refer [doit]]
   [chic.clj-editor.parser :as parser]
   [chic.clj-editor.ui :as clj-editor.ui]
   [chic.clj-editor.nav :as clj-editor.nav]
   [chic.util :as util]
   [chic.clj-editor.keybindings :as keybindings]
   [chic.clj-editor :as clj-editor]
   [chic.clj-editor.ast.string :as ast.string]
   [chic.clj-editor.ast :as ast]
   [chic.clj-editor.lines :as lines]
   [rewrite-clj.parser :as rw.p]
   [rewrite-clj.node :as rw.n]
   [rewrite-clj.zip :as rw.z]
   [io.github.humbleui.profile :as profile]
   [chic.focus :as focus]
   [io.github.humbleui.paint :as huipaint]
   [chic.text-editor :as text-editor :refer [PTextEditor_Element]]
   [chic.ui.layout :as cuilay]
   [chic.ui.focusable :as focusable]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.ui :as ui]
   [chic.paint :as cpaint]
   [chic.ui :as cui])
  (:import
   (chic.text_editor TextEditor)
   [io.github.humbleui.skija Canvas Paint TextLine PaintMode ColorFilter
    BlendMode ColorMatrix InversionMode FontMetrics Font Path]
   [io.github.humbleui.types IPoint IRect Rect RRect]
   [java.lang AutoCloseable]))

(def magenta (huipaint/fill 0xFF7A3E9D))
(def green (huipaint/fill 0xFF448C27))
(def sym-ns-colour (huipaint/fill 0xFF575250))
(def grey (huipaint/fill 0xFF777777))
(def seg-focus-background (doto (.makeClone clj-editor.ui/cursor-blue)
                            (.setAlpha 15)))

(defn ui-seg-focus-fill []
  (cui/lifecycle
   (fn after-draw [child ctx]
     (when-some [f (::clj-editor.ui/reg-ui-cursor! ctx)]
       (f child ctx)))
   (fn on-close [child unreg]
     (when unreg (unreg)))
   (cui/measured
    (cui/on-draw
     (fn [_ rect ^Canvas canvas]
       (let [height (:height rect)
             width (:width rect)]
         (.drawRRect canvas (RRect/makeXYWH 0 0 width
                                            (unchecked-subtract-int ^int height 2) 2)
                     seg-focus-background)
         (.drawPath canvas (doto (Path.)
                             (.addRect (Rect/makeXYWH 0 (unchecked-subtract-int ^int height 2)
                                                      width 2)))
                    clj-editor.ui/cursor-blue)))
     (ui/gap 0 0)))))

(defn text-segment []
  (cui/updating-ctx
   (fn [ctx]
     (assoc ctx :node (get (-> ctx ::clj-editor/state :ast ::ast/nodes)
                           (nth (:segment ctx) 1))))
   (cui/dynamic
    ctx [{:keys [fill-text font-code segment cursor-in-seg? seg-cursor-idx node]} ctx]
    (let [seg-label (fn [x]
                      (cui/lifecycle
                       (fn after-draw [child ctx]
                         (when-some [f (::clj-editor.ui/reg-ui-seg! ctx)]
                           (f child ctx)))
                       (fn on-close [child unreg]
                         (when unreg (unreg)))
                       (cui/measured
                        (cond->> (cui/dyncomp (clj-editor.ui/seg-label x font-code))
                          (::ast/transient-node? node)
                          (ui/fill (huipaint/fill (cpaint/okhsva 0.28 0.18 1.0 0.4)))))))
          ui (case (nth segment 0)
               ::ast/seg.simple
               (case (::ast/node-type node)
                 ::ast/type.symbol
                 (seg-label
                  [[(str (::ast/symbol.ns node)) sym-ns-colour]
                   [(str (::ast/symbol.name node)) fill-text]])
                 ::ast/type.keyword
                 (seg-label
                  [[(ast.string/simple-node->string node) magenta]])
                 ::ast/type.string
                 (seg-label
                  [[(ast.string/simple-node->string node) green]])
                 ::ast/type.number
                 (seg-label
                  [[(ast.string/simple-node->string node) magenta]])
                 ::ast/type.comment
                 (seg-label
                  [[(ast.string/simple-node->string node)
                    (huipaint/fill 0xFFAA3731)]])
                 ::ast/type.constant
                 (seg-label
                  [[(ast.string/simple-node->string node) magenta]])
                 ::ast/type.var
                 (seg-label
                  [["#'" fill-text]
                   [(::ast/symbol.ns node) sym-ns-colour]
                   [(::ast/symbol.name node) fill-text]])
                 ::ast/type.char
                 (seg-label
                  [[(ast.string/simple-node->string node) green]])
                  ;; Else
                 (seg-label [[(ast.string/simple-node->string node) fill-text]]))
               ::ast/seg.comp-start
               (case (::ast/node-type node)
                 (::ast/type.quote
                  ::ast/type.syntax-quote
                  ::ast/type.unquote
                  ::ast/type.unquote-splicing
                  ::ast/type.deref
                  ::ast/type.var)
                 (seg-label
                  [[(ast.string/composite-node->string-start node)
                    (huipaint/fill 0xFF333333)]])
                  ;; Else
                 (seg-label
                  [[(ast.string/composite-node->string-start node)
                    grey]]))
               ::ast/seg.comp-end
               (seg-label
                [[(ast.string/composite-node->string-end node)
                  grey]]))]
      (if (and cursor-in-seg? (nil? seg-cursor-idx))
        (cuilay/stack
         (cui/dyncomp (ui-seg-focus-fill))
         ui)
        ui)))))

(defn ui-empty-line []
  (ui/dynamic ctx
    [{:keys [cursor-in-line? seg-cursor-idx]} ctx]
    (cond
      seg-cursor-idx
      (cui/dyncomp (clj-editor.ui/ui-insert-cursor 2))
      cursor-in-line?
      (cuilay/width
       6 (cui/dyncomp (ui-seg-focus-fill)))
      true
      (ui/gap 0 0))))

(defn full-text-line []
  (cui/lifecycle
   (fn [child ctx]
     (when-some [f (::clj-editor.ui/reg-ui-line! ctx)]
       (f child ctx)))
   (fn [_ unreg]
     (when unreg (unreg)))
   (cui/measured
    (cui/dynamic
     ctx [{:keys [line-nodes font-code]} ctx]
     (cuilay/row
      (ui/gap 0 (Math/ceil (* 1.1 (.getHeight (.getMetrics ^Font font-code)))))
      (if (== 0 (count line-nodes))
        (cui/updating-ctx
         (fn [ctx]
           (-> ctx
               (assoc :seg-cursor-idx (:local-idx (:line-cursor ctx)))))
         (cui/dyncomp (ui-empty-line)))
        (eduction
         (map-indexed
          (fn [idx segment]
            (cui/updating-ctx
             (fn [ctx]
               (-> ctx
                   (assoc :segment segment)
                   (assoc :seg-idx idx)
                   (cond-> (= idx (:seg-idx (:line-cursor ctx)))
                     (-> (assoc :cursor-in-seg? true)
                         (assoc :seg-cursor-idx (:local-idx (:line-cursor ctx)))))))
             (cui/dyncomp (text-segment)))))
         line-nodes)))))))

(defn main-view []
  (cui/dynamic
   ctx [{:keys []
         {line-order ::ast/line-order} :ast} (::clj-editor/state ctx)
         ;; focus-manager (:focus-manager ctx)
         ;; window (:chic/current-window ctx)
        ]
   (let [ctor (fn [line-id]
                (let [ui (cui/measure-once
                          (cui/updating-ctx
                           (fn [ctx]
                             (let [state (::clj-editor/state ctx)
                                   cursor (:cursor state)]
                               (-> ctx
                                   (assoc :line-id line-id)
                                   (assoc :line-nodes (get (::ast/lines (:ast state)) line-id))
                                   (cond-> (= line-id (:line cursor))
                                     (-> (assoc :cursor-in-line? true)
                                         (assoc :line-cursor cursor))))))
                           (cui/dyncomp (full-text-line))))]
                  [:hug nil ui]))]
     (cui/lifecycle
      (fn after-draw [child ctx]
        (when-some [f (::clj-editor.ui/reg-vscroll! ctx)]
          (f child ctx)))
      (fn on-close [child unreg]
        (when unreg (unreg)))
      (cuilay/vscroll
       (cui/responder
        {::clj-editor.ui/get-line-rects
         (fn [child _]
           (:child-rects child))}
        (cuilay/column
         (eduction
          (map ctor)
          line-order)))))
     #_(clj-editor.ui/virtual-vscroll
        line-order
        ctor
        (ui/gap 0 0)))))

(defn al-ensure-idx-exists [^java.util.ArrayList lst ^long idx]
  (let [diff (prim/- (prim/inc idx) (.size lst))]
    (when (prim/< 0 diff)
      (dotimes [_ diff]
        (.add lst nil)))))

(deftype+ BasicCljEditor [^java.util.HashMap line->segs
                          ^:mut ui-vscroll ^:mut ui-cursor
                          ^java.util.HashMap ui-lines
                          ^java.util.HashMap responders
                          ^java.util.HashMap responders-by-msg child]
  IComponent
  (-measure [_ ctx rect]
    (huip/-measure child ctx rect))

  (-draw [self ctx rect ^Canvas canvas]
    (huip/-draw
     child (assoc ctx
                  ::cui/install-responder
                  (fn [rmap rchild rctx]
                    (let [rmap (update-keys
                                rmap
                                (fn [msg]
                                  (case msg
                                    ::clj-editor.ui/cursor-rect->idx
                                    [::clj-editor.ui/cursor-rect->idx.for-seg
                                     (:line-id rctx) (:seg-idx rctx)]
                                    msg)))]
                      (.put responders rchild rmap)
                      (doit [k (keys rmap)]
                        (.put responders-by-msg k rchild))
                      (fn uninstall []
                        (doit [k (keys rmap)]
                          (when (identical? rchild (.get responders-by-msg k))
                            (.remove responders-by-msg k)))
                        (.remove responders rchild))))

                  ::clj-editor.ui/reg-ui-seg!
                  (fn [ui-seg {:keys [line-id seg-idx]}]
                    (if-some [segs (.getOrDefault line->segs line-id nil)]
                      (do (al-ensure-idx-exists segs seg-idx)
                          (.set ^java.util.ArrayList segs seg-idx ui-seg))
                      (let [segs (java.util.ArrayList.)]
                        (al-ensure-idx-exists segs seg-idx)
                        (.set segs seg-idx ui-seg)
                        (.put line->segs line-id segs)))
                    (fn unreg []))
                  ::clj-editor.ui/reg-ui-line!
                  (fn [ui-line {:keys [line-id]}]
                    (.put ui-lines line-id ui-line)
                    (fn [] (.remove ui-lines line-id)))
                  ::clj-editor.ui/reg-vscroll!
                  (fn [vscroll _]
                    (hui/-set! self :ui-vscroll vscroll)
                    nil)
                  ::clj-editor.ui/reg-ui-cursor!
                  (fn [ui-cursor _]
                    (hui/-set! self :ui-cursor ui-cursor)
                    nil))
     rect canvas))

  (-event [self event]
    (hui/eager-or (when-some [f ({:hui/key-down keybindings/handle-keydown
                                  :hui/key-up keybindings/handle-keyup}
                                 (:hui/event event))]
                    (f self (:ctx event) event))
                  (huip/-event child event)))

  AutoCloseable (close [_] (ui/child-close child)))

(defn make [{:keys [content]}]
  (let [ast (parser/read-fresh (java.io.StringReader. content))
        *state (atom {:ast ast
                      :cursor {:line (first (::ast/line-order ast))
                               :seg-idx 0
                               :local-idx 0}})
        *old-states (atom {:transient-idx nil
                           :states []})
        backup-state! (fn [*state]
                        (swap! *old-states
                               (fn [m]
                                 (-> m
                                     (cond-> (:transient-idx m)
                                       (-> (assoc :transient-idx nil)
                                           (update :states subvec 0 (inc (:transient-idx m)))))
                                     (update :states conj @*state)))))
        ui-editor-dc (cui/dyncomp
                      (->BasicCljEditor
                       (java.util.HashMap.)
                       nil
                       nil
                       (java.util.HashMap.)
                       (java.util.HashMap.)
                       (java.util.HashMap.)
                       (cui/dyncomp (main-view))))
        self (:child ui-editor-dc)
        focus-node (random-uuid)
        move-to-handler
        (fn [info]
          (let [state (:state info)]
            (when-some [pos (clj-editor.nav/resolve-pos
                             self (:ast state) (:cursor state) (:pos info))]
              (swap! *state assoc :cursor pos)
              (clj-editor.nav/ensure-cursor-in-view self (:ast state) pos))
            nil))
        delete-simple-node
        (fn [node-id pos]
          (swap! *state update :ast
                 (fn [ast]
                   (-> ast (update ::ast/nodes dissoc node-id)
                       (update-in [::ast/lines (:line pos)]
                                  (fn [segs]
                                    (into (subvec segs 0 (:seg-idx pos))
                                          (subvec segs (inc (:seg-idx pos))))))))))
        replace-simple-node
        (fn [node-id node]
          (swap! *state update :ast
                 (fn [ast]
                   (-> ast (update ::ast/nodes assoc node-id node)))))]
    (cui/updating-ctx
     (fn [ctx]
       (assoc ctx ::clj-editor/state @*state))
     (cui/on
      ::clj-editor/undo
      (fn [_]
        (let [old-states (swap! *old-states
                                (fn [m]
                                  (update m :transient-idx
                                          (fn [i] (max 0 (dec (or i (count (:states m)))))))))
              idx (:transient-idx old-states)
              states (:states old-states)]
          (when (and idx (seq states))
            (reset! *state (nth states idx)))))
      (cui/on
       ::clj-editor/ast.delete-simple-node
       (fn [{:keys [pos node-id ::clj-editor/move-to]}]
         (backup-state! *state)
         (when move-to (move-to-handler move-to))
         (delete-simple-node node-id pos))
       (cui/on
        ::clj-editor/ast.replace-simple-node
        (fn [{:keys [pos node-id new-node ::clj-editor/move-to]}]
          (backup-state! *state)
          (when move-to (move-to-handler move-to))
          (replace-simple-node node-id new-node))
        (cui/on
         ::clj-editor/ast.update-node
         (fn [{:keys [node-id new-node ::clj-editor/move-to]}]
           (backup-state! *state)
           (when move-to (move-to-handler move-to))
           (swap! *state assoc-in [:ast ::ast/nodes node-id] new-node))
         (cui/on
          ::clj-editor/move-to
          move-to-handler
          (cui/on-event
           (fn [event]
             (when (= :hui/text-input (:hui/event event))
               (keybindings/handle-text-input self (:ctx event) event)))
           (focusable/make
            {:focus-node focus-node}
            ui-editor-dc))))))))))

(comment
  (let [s (slurp "src/chic/graph.clj")
        z (time (rw.z/edn (rw.p/parse-string s)))]
    (time (rw.z/prewalk z (fn [_]))))
  (rw.n/children (rw.p/parse-string "( ,\t)"))
  (rw.n/children (rw.p/parse-string "#(:f % a)"))
  (rw.n/children (rw.p/parse-string "#helo []"))
  (rw.p/parse-string "#_#_[][]")
  (rw.p/parse-string "#_[]")
  (rw.p/parse-string "#?(:clj 4)")
  (rw.n/children (rw.p/parse-string "#:x {:y 8}"))
  (rw.p/parse-string "\"x\n \" ")

  (require '[clj-kondo.core :as kondo])
  (binding [*in* (java.io.StringReader. "()")]
    (:analysis
     (kondo/run! {:lint [(clojure.java.io/file "-")]
                  :cache true
                  :config {:output {:analysis {:arglists true
                                               :locals true
                                               :keywords false
                                               :protocol-impls true
                                               :java-class-definitions true}
                                    :canonical-paths true}}})))

  (let [s (slurp "src/chic/graph.clj")
        m (re-matcher #"\n" s)]
    (time (dotimes [_ 200] (re-find m))))

#!
  )
