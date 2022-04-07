(ns chic.text-editor.move
  (:require
   [io.github.humbleui.protocols :as huip]
   [chic.windows :as windows]
   [chic.text-editor :as text-editor :refer [PTextEditor_Move]]
   [chic.text-editor.cursor :as cursor]
   [chic.text-editor.element]
   [chic.text-editor.line :as line])
  (:import
   (chic.text_editor TextEditor)
   (chic.text_editor.element TextLineSegment)))

(defn move-up-or-down [*state up?]
  (let [sm @*state]
    (reduce
    (fn [sm {:keys [idx i line-id] :as cursor}]
      (if-let [next-line-id ((if up? line/prev-line line/next-line) sm line-id)]
        (let [lines-by-id (:lines-by-id sm)
              this-line (get lines-by-id line-id)
              next-line (get lines-by-id next-line-id)
              #_#_#_#_#_#_#_#_#_#_#_#_#_#_
              x (loop [seg-i 0
                       width-acc 0]
                  (let [seg (nth segs seg-i)]
                    (let [seg-width (:x (:size (:ui seg)))]
                      (if (= i (:cursor-id seg))
                        (if (:insert-mode? sm)
                          width-acc
                          (unchecked-add-int width-acc (int (/ seg-width 2))))
                        (recur (unchecked-inc-int seg-i)
                               (unchecked-add-int width-acc seg-width))))))
              next-line (get-in sm [:lines-by-id next-line-id])
              next-segs (:ui-segments next-line)
              text-seg? (fn [seg] (instance? TextLineSegment (:ui seg)))
              [target-seg target-seg-offset]
              (loop [seg-i 0
                     width-acc 0
                     prev-text-seg-and-bounds nil]
                (if-let [seg (nth next-segs seg-i nil)]
                  (let [seg-width (:x (:size (:ui seg)))
                        next-width (unchecked-add-int width-acc seg-width)]
                    (if (< next-width x)
                      (recur (unchecked-inc-int seg-i)
                             (unchecked-add-int width-acc seg-width)
                             (if (text-seg? seg)
                               [seg width-acc next-width]
                               prev-text-seg-and-bounds))
                      (if (text-seg? seg)
                        [seg width-acc]
                        (loop [seg-i seg-i
                               width-acc width-acc]
                          (if-let [ahead-seg (nth next-segs seg-i nil)]
                            (if (text-seg? ahead-seg)
                              (if (> (unchecked-subtract-int width-acc x)
                                     (unchecked-subtract-int x (nth prev-text-seg-and-bounds 2)))
                                (pop prev-text-seg-and-bounds)
                                [ahead-seg width-acc])
                              (recur (unchecked-inc-int seg-i)
                                     (unchecked-add-int width-acc (:x (:size (:ui seg))))))
                            (pop prev-text-seg-and-bounds))))))
                  (pop prev-text-seg-and-bounds)))
              closest-info ((:closest-char-fn target-seg) (- x target-seg-offset))
              target-idx (unchecked-add-int
                          (:start-idx target-seg)
                          (cond-> (:local-idx closest-info)
                            (and (:insert-mode? sm) (:right-side? closest-info))
                            (-> inc (min (line/end-idx next-line)))))]
          (windows/send-event
           (:window this-line)
           {:chic.text-editor/event.cursor->x.id 0
            :chic.text-editor/event.cursor->x.callback
            (fn [x]
              (windows/send-event
               (:window next-line)
               {:chic.text-editor/event.x->char.x x
                :target-line-id next-line-id
                :chic.text-editor/event.x->char.callback
                (fn [{:keys [local-idx]}]
                  (swap! *state assoc-in [:cursors i]
                         (assoc cursor
                                :idx (min local-idx (dec (count (:content next-line))))
                                :line-id next-line-id)))}))}))
        sm))
    sm
    (cursor/cursors-indexed sm))))

(defn move-backwards [sm]
  (reduce (fn [sm {:keys [idx i line-id]}]
            (if (zero? idx)
              sm
              (update-in sm [:cursors i :idx] dec)))
          sm
          (cursor/cursors-indexed sm)))

(extend-type TextEditor
  PTextEditor_Move
  (move-up [{:keys [state]}]
    (move-up-or-down state true))
  (move-down [{:keys [state]}]
    (move-up-or-down state false))
  (move-forwards [{:keys [state] :as self}]
    (swap! state (fn [sm]
                   (reduce (fn [sm {:keys [idx i line-id]}]
                             (if (== idx (cond-> (line/end-idx (get-in sm [:lines-by-id line-id]))
                                           (:insert-mode? sm) inc))
                               sm
                               (update-in sm [:cursors i :idx] inc)))
                           sm
                           (cursor/cursors-indexed sm)))))
  (move-backwards [{:keys [state] :as self}]
    (swap! state move-backwards)))
