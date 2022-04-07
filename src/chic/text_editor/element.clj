(ns chic.text-editor.element
  (:require
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
   [chic.ui :as cui])
  (:import
   (chic.text_editor TextEditor)
   [io.github.humbleui.skija Canvas Paint TextLine ColorFilter BlendMode ColorMatrix InversionMode FontMetrics]
   [io.github.humbleui.types IPoint IRect Rect RRect]
   [java.lang AutoCloseable]))

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

(defn text-run-segment []
  (cui/updating-ctx
   (let [*cache-label (volatile! nil)
         *cache-input (volatile! nil)]
     (fn [ctx]
       (let [{:keys [font-code fill-text segment-content]} ctx
             input [font-code fill-text segment-content]]
         (assoc ctx :ui-label
                (or (if (= input @*cache-input)
                      @*cache-label
                      (do (vreset! *cache-input input) nil))
                    (do (ui/child-close @*cache-label)
                        (vreset! *cache-label (ui/label (if (== 0 (count segment-content))
                                                          " "
                                                          segment-content)
                                                        font-code fill-text))))))))
   (cui/on-event
    (fn [event]
      (when-let [x (:chic.text-editor/event.x->char.x event)]
        (when (= (:line-id event) (:target-line-id event))
          (let [textline ^TextLine (:line (:ui-label event))]
            ((:chic.text-editor/event.x->char.callback event)
             {:local-idx (if (:insert-mode? (::text-editor/state event))
                           (.getOffsetAtCoord textline x)
                           (.getLeftOffsetAtCoord textline x))}))))
      false)
    (let [top-padding 3
          bottom-padding 5
          base (fn [] (cuilay/padding
                       0 top-padding 0 bottom-padding
                       (ui/contextual (fn [ctx] (:ui-label ctx)))))]
      (cui/dynamic
       ctx [cursor-id (when (= (:line-id ctx)
                               (:line-id (nth (:cursors (::text-editor/state ctx)) 0)))
                        0)]
       (if cursor-id
         (cui/updating-ctx
          (fn [ctx]
            (let [textline ^TextLine (:line (:ui-label ctx))
                  cursor-idx (max 0 (:idx (get (:cursors (::text-editor/state ctx)) cursor-id)))]
              (assoc ctx
                     :char-left (.getCoordAtOffset textline cursor-idx)
                     :char-right (.getCoordAtOffset textline (unchecked-inc-int cursor-idx)))))
          (let [normal-background (huipaint/fill 0xFFD07040)
                insert-background (huipaint/fill 0xFF4070D0)
                insert-foreground (huipaint/fill 0x70000000)
                normal-foreground (huipaint/fill 0xFFFFFFFF)]
            (cuilay/stack
             (base)
             (let [*cursor-x (volatile! 0)]
               (cui/on-event
                (fn [event]
                  (when (= cursor-id (:chic.text-editor/event.cursor->x.id event))
                    ((:chic.text-editor/event.cursor->x.callback event)
                     @*cursor-x))
                  false)
                (cui/on-draw
                 (fn [{:keys [ui-label char-left char-right]
                       {:keys [insert-mode?]} ::text-editor/state} cs ^Canvas canvas]
                   (let [layer (.save canvas)]
                     (if insert-mode?
                       (do (.drawRect canvas (Rect/makeLTRB char-left 0 (unchecked-add-int 2 char-left) (:height cs))
                                      insert-background)
                           (vreset! *cursor-x char-left))
                       (do (.drawRRect canvas (RRect/makeLTRB char-left 0 char-right (:height cs) 2)
                                       normal-background)
                           (vreset! *cursor-x (unchecked-int (/ (unchecked-add-int char-left char-right) 2)))))
                     (.clipRect canvas (if insert-mode?
                                         (Rect/makeLTRB char-left 0 (unchecked-add-int 2 char-left) (:height cs))
                                         (RRect/makeLTRB char-left 0 char-right (:height cs) 2)))
                     (.drawTextLine
                      canvas (:line ui-label) 0
                      (unchecked-add-int top-padding (Math/ceil (.getCapHeight ^FontMetrics (:metrics ui-label))))
                      (if insert-mode?
                        insert-foreground
                        normal-foreground))
                     (.restoreToCount canvas layer)))
                 (ui/gap 0 0)))))))
         (base)))))))

(defn full-text-line [line-id]
  (cui/updating-ctx
   (fn [ctx] (assoc ctx :line-id line-id
                    :segment-content
                    (:content (get (:lines-by-id (::text-editor/state ctx))
                                   line-id))))
     (cuilay/row
      (cui/dyncomp (text-run-segment))
      nil)))

(extend-type TextEditor
  PTextEditor_Element
  (element [{:keys [state focus-node] :as self}]
    (cui/updating-ctx
     (fn [ctx]
       (assoc ctx ::text-editor/state @state))
     (focusable/make
      {:focus-node focus-node}
      (cui/dynamic
       ctx [{:keys [line-order]} (::text-editor/state ctx)
            focus-manager (:focus-manager ctx)
            window (:chic/current-window ctx)]
       (let [new-lines (transient (:lines-by-id @state))
             ui-lines (mapv (fn [line-id]
                              (let [ui (cui/dyncomp (full-text-line line-id))]
                                (assoc! new-lines line-id
                                        (assoc (get new-lines line-id)
                                               :ui ui
                                               :window window))
                                [:hug nil ui]))
                            line-order)]
         (swap! state assoc :lines-by-id (persistent! new-lines))
         (ui/on-key-down
          #(editor-on-key-down self focus-manager %)
          (cuilay/column
           (eduction ui-lines)))))))))

;; problem with curren implementation: letters move around when moving the cursor
;; this is because Label allocates pixel space with Math/ceil to ensure it fits
;; A text line is drawn starting at a point aligned with a pixel.
;; But the width of the first character may be fractional.
;; So when moving the cursor forwards, the starting offset of the second character is different
;; - it moves right to give full pixel space to the first character
;; not enough to modify Label to remove ceil because IPoint uses ints
