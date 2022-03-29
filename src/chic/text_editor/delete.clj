(ns chic.text-editor.delete
  (:require
   [chic.text-editor :as text-editor :refer [PTextEditor_Delete]]
   [chic.text-editor.cursor :as cursor]
   [taoensso.encore :as enc]
   [chic.text-editor.line :as line]
   [chic.text-editor.misc :as misc]
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
   [io.github.humbleui.ui :as ui])
  (:import
   (chic.text_editor TextEditor)
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventMouseScroll
    EventKey Window Key KeyModifier]
   [io.github.humbleui.skija Canvas FontMgr FontStyle Typeface Font Paint PaintMode TextLine]
   [io.github.humbleui.types IPoint]))

(defn line-delete-between* [sm line-id fromidx uptoidx]
  (update-in sm [:lines-by-id line-id]
             (fn [{:keys [content] :as line}]
               (assoc line :content
                      (str (subs content 0 fromidx)
                           (subs content uptoidx))))))

(defn delete-line [sm line-id]
  (-> sm
      (assoc :line-order (persistent! (enc/into! (transient [])
                                                 (remove #{line-id})
                                                 (:line-order sm))))
      (update :lines-by-id dissoc line-id)))

(extend-type TextEditor
  PTextEditor_Delete
  (delete-up-to [{:keys [state]} uptoidx]
    (swap! state
           (fn [sm]
             (reduce (fn [sm {:keys [idx line-id i]}]
                       (if (< uptoidx idx)
                         (-> sm
                             (line-delete-between* line-id uptoidx idx)
                             (assoc-in [:cursors i :idx] uptoidx))
                         (line-delete-between* sm line-id idx uptoidx)))
                     sm
                     (cursor/cursors-indexed sm)))))
  (delete-backwards [{:keys [state]} n]
    (swap! state (fn [{:keys [] :as sm}]
                   (reduce (fn [sm {:keys [idx line-id i]}]
                             (if (zero? idx)
                               (let [line-order (:line-order sm)
                                     line-before-id (nth line-order (unchecked-dec-int (line/line-order-idx sm line-id)) nil)]
                                 (if line-before-id
                                   (let [line-before (get-in sm [:lines-by-id line-before-id])
                                         content-before (:content line-before)]
                                     (-> sm
                                         (delete-line line-before-id)
                                         (update-in [:lines-by-id line-id :content]
                                                    (fn [c] (str content-before c)))
                                         (assoc-in [:cursors i :idx] (count content-before))))
                                   sm))
                               (-> sm
                                   (line-delete-between* line-id (unchecked-subtract-int idx n) idx)
                                   (assoc-in [:cursors i :idx] (unchecked-subtract-int idx n)))))
                           sm
                           (cursor/cursors-indexed sm)))))
  (delete-forwards [{:keys [state]} n]
    (swap! state
           (fn [sm]
             (reduce (fn [sm {:keys [idx line-id i]}]
                       (let [line (get-in sm [:lines-by-id line-id])
                             line-end-idx (line/end-idx line)]
                         (if (== (unchecked-dec-int idx) line-end-idx)
                           (let [line-order (:line-order sm)
                                 line-after-id (nth line-order (inc (line/line-order-idx sm line-id)) nil)]
                             (if line-after-id
                               (let [line-after (get-in sm [:lines-by-id line-after-id])
                                     content-after (:content line-after)]
                                 (-> sm
                                     (delete-line line-after-id)
                                     (update-in [:lines-by-id line-id :content]
                                                (fn [c] (str c content-after)))))
                               sm))
                           (-> sm
                               (line-delete-between* line-id idx (+ idx n))
                               (cond-> (and (not (:insert-mode? sm)) (< line-end-idx (+ idx n)))
                                 (assoc-in [:cursors i :idx] (max 0 (dec line-end-idx))))))))
                     sm
                     (cursor/cursors-indexed sm))))))
