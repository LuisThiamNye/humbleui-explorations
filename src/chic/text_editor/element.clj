(ns chic.text-editor.element
  (:require
   [clojure.math :as math]
   [chic.text-editor :as text-editor :refer [PTextEditor_Element PTextEditor_Move]]
   [clojure.string :as str]
   [chic.key :as key]
   [chic.focus :as focus]
   [chic.ui.focusable :as focusable]
   [chic.keybindings :as keybindings]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.paint :as paint]
   [io.github.humbleui.profile :as profile]
   [nrepl.cmdline :as nrepl]
   [io.github.humbleui.window :as window]
   [io.github.humbleui.ui :as ui]
   [io.github.humbleui.protocols :as huip :refer [IComponent]])
  (:import
   [java.lang AutoCloseable]
   (chic.text_editor TextEditor)
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventMouseScroll
    EventKey Window Key KeyModifier]
   [io.github.humbleui.skija Canvas FontMgr FontStyle Typeface Font Paint PaintMode TextLine]
   [io.github.humbleui.types IPoint IRect Point Rect RRect]))

(defn editor-on-key-down [self focus-manager evt]
  (when (focus/has-focus? focus-manager (:focus-node self))
    (some #((:handler %) self evt)
          (reverse (:keydown-handlers @(:state self))))))

;;
;; must compute the AST
;; what does rewrite clj do?
;; for each line, convert part of AST to components.
;;   the AST is linearised and processed sequentially
;;     until no pixel space is left per line segment
;; where text is split, this is two components sharing an AST node.
;; maybe a parallel data structure of content by line
;;    each line is a scoped view of the linearised AST tree
;; default ast can just be a single node of the entire string or streaming equivalent
;;    or makes more sense to have one node per line
;;

(comment
  (require '[rewrite-clj.parser]
           '[rewrite-clj.node])
  (get
   (rewrite-clj.node/children
    (rewrite-clj.parser/parse-string "(x \r\n\r\n  4)"))
   3)
  ;; seems like carriage return gets ignored and newlines are treated as a single node.
  ;; so would be good to abstract over the specific characters
  ;; when dealing with the data in memory
  ;; and just apply the correct characters for each platform
  ;; there is no good reason to mix crlf and lf
  ;; crlf vs lf is just an implementation detail/historical accident that noone should care about

  #!
  )

(deftype+ NonTextLineSegment [child ^:mut size ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
            (huip/-measure child ctx cs))

  (-draw [_ ctx cs ^Canvas canvas]
         (set! size cs)
         (set! child-rect (IRect/makeXYWH 0 0 (:width cs) (:height cs)))
         (huip/-draw child ctx cs canvas))

  (-event [_ event]
          (ui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
         (ui/child-close child)))

(deftype+ TextLineSegment [child ^:mut size ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
            (huip/-measure child ctx cs))

  (-draw [_ ctx cs ^Canvas canvas]
         (set! size cs)
         (set! child-rect (IRect/makeXYWH 0 0 (:width cs) (:height cs)))
         (huip/-draw child ctx cs canvas))

  (-event [_ event]
          (ui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
         (ui/child-close child)))

(defn text-line-segment [child]
  (->TextLineSegment child nil nil))

(deftype+ FullTextLine [children ^:mut child-rects]
  IComponent
  (-measure [_ ctx cs]
            ;; should not overflow horizontally normally
            ;; so -measure should return the real size within the cs
            ;; accounting for text wrapping
            (reduce
             (fn [{:keys [width height]} child]
               (let [child-size (huip/-measure child ctx cs)]
                 (IPoint. (+ width (:width child-size)) (max height (:height child-size)))))
             (IPoint. 0 0)
             (keep #(nth % 2) children)))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [known (for [[mode _ child] children]
                       (when (= :hug mode)
                         (huip/-measure child ctx cs)))
               space (- (:width cs) (transduce (keep :width) + 0 known))
               stretch (transduce (keep (fn [[mode value _]] (when (= :stretch mode) value))) + 0 children)
               layer (.save canvas)]
           (try
             (loop [width 0
                    rects []
                    known known
                    children children]
               (if-some [[mode value child] (first children)]
                 (let [child-size (case mode
                                    :hug (first known)
                                    :stretch (IPoint. (-> space (/ stretch) (* value) (math/round)) (:height cs)))]
                   (when child
                     (huip/-draw child ctx (assoc child-size :height (:height cs)) canvas))
                   (.translate canvas (:width child-size) 0)
                   (recur
                    (+ width (long (:width child-size)))
                    (conj rects (IRect/makeXYWH width 0 (:width child-size) (:height cs)))
                    (next known)
                    (next children)))
                 (set! child-rects rects)))
             (.restoreToCount canvas layer))))

  (-event [_ event]
          (reduce
           (fn [acc [[_ _ child] rect]]
             (hui/eager-or acc (ui/event-propagate event child rect) false))
           false
           (hui/zip children child-rects)))

  AutoCloseable
  (close [_]
         (doseq [[_ _ child] children]
           (ui/child-close child))))

(defn cursor-segment [{:keys [insert-mode?]} {:keys [font-code fill-text vpadding] :as ctx} s cursor-idx cursor-id]
  (let [label (if insert-mode?
                (ui/label s font-code (doto (Paint.) (.setColor (unchecked-int 0xFFFFFFFF))))
                (ui/label s font-code (doto (Paint.) (.setColor (unchecked-int 0xFFFFFFFF)))))]
    {:start-idx cursor-idx
     :cursor-id cursor-id
     :closest-char-fn (fn [x]
                        {:local-idx 0
                         :right-side? (< (/ (.getWidth ^TextLine (:line label)) 2) x)})
     :ui
     (text-line-segment
      (if insert-mode?
        (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFF4070D0)))
                 (ui/padding
                  0 vpadding
                  label))
        (ui/clip-rrect
         2
         (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFD07040)))
                  (ui/padding
                   0 vpadding
                   label)))))}))

(defn text-run-segment [{:keys [font-code fill-text vpadding]} content start-idx]
  (let [label (ui/label content font-code fill-text)]
    {:start-idx start-idx
     :closest-char-fn (fn [x]
                        (let [guessed-idx (math/round (* (unchecked-dec-int (count content))
                                                         (min 1 (/ x (.getWidth ^TextLine (:line label))))))]
                          {:local-idx guessed-idx
                           :right-side? false}))
    :ui
    (text-line-segment
     (ui/padding
      0 vpadding
      label))}))

(defn full-text-line [self {:keys [font-code fill-text vpadding] :as ctx} line-id]
  (let [sm @(:state self)
        line (get-in sm [:lines-by-id line-id])
        content (:content line)
        cursor (first (:cursors sm))
        cursor-id 0
        segments (if (= line-id (:line-id cursor))
                   (let [cursor-idx (:idx cursor)]
                     [(text-run-segment ctx (subs content 0 cursor-idx) 0)
                      (cursor-segment sm ctx (str (nth content cursor-idx \space)) cursor-idx cursor-id)
                      (text-run-segment ctx (subs content (min (inc cursor-idx) (count content))) (unchecked-dec-int (count content)))])
                   [(text-run-segment ctx content 0)])]
    (swap! (:state self) assoc-in [:lines-by-id line-id :ui-segments]
           segments)
    (ui/row
     (ui/fill (doto (Paint.) (.setColor (unchecked-int 0x9F4070a0)))
              (->FullTextLine
               (-> (mapv (fn [s] [:hug nil (:ui s)]) segments)
                   #_(conj [:stretch 1 (ui/gap 0 0)]))
               nil))
     [:stretch 1 (ui/gap 0 0)])))

(extend-type TextEditor
  PTextEditor_Element
  (element [{:keys [state focus-node] :as self}]
    (focusable/make
     {:focus-node focus-node}
     (ui/dynamic
      ctx [scale (:scale ctx)
           font-code (:font-code ctx)
           state' @state
           focus-manager (:focus-manager ctx)]
      (let [scale scale
            font-code font-code
            {:keys [line-order]} state'
            leading (-> font-code .getMetrics .getCapHeight Math/ceil (/ scale))
            fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
            vpadding (/ leading 2)
            ctx {:fill-text fill-text :font-code font-code :vpadding vpadding}]
        (ui/on-key-down
         #(editor-on-key-down self focus-manager %)
         (ui/column
          (for [line-id line-order]
            (full-text-line self ctx line-id)))))))))

;; problem with curren implementation: letters move around when moving the cursor
;; this is because Label allocates pixel space with Math/ceil to ensure it fits
;; A text line is drawn starting at a point aligned with a pixel.
;; But the width of the first character may be fractional.
;; So when moving the cursor forwards, the starting offset of the second character is different
;; - it moves right to give full pixel space to the first character
