(ns chic.text-editor.keybindings
  (:require
   [chic.text-editor :as text-editor :refer [PTextEditor_Modes]]
   [chic.text-editor.cursor :as cursor]
   [chic.text-editor.move :as move]
   [chic.text-editor.line :as line]
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
   [io.github.humbleui.skija Canvas FontMgr FontStyle Typeface Font Paint PaintMode]
   [io.github.humbleui.types IPoint]))

(defn handle-keydown-normal-mode [self evt]
  (let [ek ^EventKey (:eventkey evt)
        k ^Key (.getKey ek)]
    (or
     (when (key/no-modifiers? ek)
       (cond
         (identical? Key/L k)
         [(text-editor/move-forwards self)]
         (identical? Key/H k)
         [(text-editor/move-backwards self)]
         (identical? Key/J k)
         [(text-editor/move-down self)]
         (identical? Key/K k)
         [(text-editor/move-up self)]
         (identical? Key/W k)
         [(swap! (:state self)
                 (fn [sm]
                   (cursor/move-cursor-to-next-line-idx sm line/next-start-of-word-idx)))]
         (identical? Key/B k)
         [(swap! (:state self)
                 (fn [sm]
                   (cursor/move-cursor-to-prev-line-idx sm line/prev-start-of-word-idx)))]
         (identical? Key/E k)
         [(swap! (:state self)
                 (fn [sm]
                   (cursor/move-cursor-to-next-line-idx sm line/next-end-of-word-idx)))]
         (identical? Key/I k)
         [(text-editor/enable-insert-mode self)]
         (identical? Key/X k)
         [(text-editor/delete-forwards self 1)]
         (identical? Key/DIGIT0 k)
         [(swap! (:state self)
                 (fn [sm]
                   (cursor/move-cursors-to-line-idx sm (constantly 0))))]
         (identical? Key/BACKSPACE k)
         [(text-editor/delete-backwards self 1)]))
     (when (key/only-modifier? ek (key/mask KeyModifier/SHIFT))
       (cond
         (identical? Key/W k)
         [(swap! (:state self)
                 (fn [sm]
                   (cursor/move-cursor-to-next-line-idx sm line/next-start-of-big-word-idx)))]
         (identical? Key/B k)
         [(swap! (:state self)
                 (fn [sm]
                   (cursor/move-cursor-to-prev-line-idx sm line/prev-start-of-big-word-idx)))]
         (identical? Key/E k)
         [(swap! (:state self)
                 (fn [sm]
                   (cursor/move-cursor-to-next-line-idx sm line/next-end-of-big-word-idx)))]
         (identical? Key/DIGIT4 k)
         [(swap! (:state self)
                 (fn [sm]
                   (cursor/move-cursors-to-line-idx sm (fn [l _] (line/end-idx l)))))]
         (identical? Key/DIGIT6 k)
         [(swap! (:state self)
                 (fn [sm]
                   (cursor/move-cursors-to-line-idx sm (constantly 0))))])))))

(defn handle-keydown-insert-mode [self evt]
  (let [ek (:eventkey evt)
        k (.getKey ^EventKey ek)]
    (or (when (key/no-modifiers? ek)
          (cond
            (or (.isLetterKey k) (.isDigitKey k))
            (text-editor/insert-str self (str/lower-case (.getName k)))
            (identical? Key/SPACE k)
            (text-editor/insert-str self " ")
            (identical? Key/TAB k)
            (text-editor/insert-str self "\t")
            (identical? Key/ENTER k)
            (text-editor/insert-str self "\n")
            (identical? Key/BACKSPACE k)
            [(text-editor/delete-backwards self 1)]
            (identical? Key/ESCAPE k)
            [(text-editor/enable-normal-mode self)]))
        (when (key/only-modifier? ek (key/mask KeyModifier/SHIFT))
          (cond
            (.isLetterKey k)
            (text-editor/insert-str self (.getName k))
            (.isDigitKey k)
            (text-editor/insert-str self (case (first (.getName k))
                                           \1 "!"
                                           \2 "@"
                                           \3 "#"
                                           \4 "$"
                                           \5 "%"
                                           \6 "^"
                                           \7 "&"
                                           \8 "*"
                                           \9 "("
                                           \0 ")"))))
        (when (key/only-modifier? ek (key/mask KeyModifier/CONTROL))
          (cond
            (identical? Key/D k)
            [(text-editor/delete-forwards self 1)]
            (or (identical? Key/G k) (identical? Key/OPEN_BRACKET k))
            [(text-editor/enable-normal-mode self)])))))

(defn remove-key-handler [state up-or-down id]
  (update state (case up-or-down :down :keydown-handlers)
          (fn [handlers]
            (filterv #(not= id (:id %)) handlers))))

(defn add-key-hanlder [state up-or-down handler]
  (update state (case up-or-down :down :keydown-handlers)
          conj handler))

(defn handle-keydown-default [self evt]
  (let [ek ^EventKey (:eventkey evt)
        k ^Key (.getKey ek)]
    (or (when (key/no-modifiers? ek)
          (cond
            (identical? Key/RIGHT k)
            [(text-editor/move-forwards self)]
            (identical? Key/LEFT k)
            [(text-editor/move-backwards self)]
            (identical? Key/DOWN k)
            [(text-editor/move-down self)]
            (identical? Key/UP k)
            [(text-editor/move-up self)]))
        (when (key/only-modifier? ek (key/mask KeyModifier/CONTROL))
          (cond
            (identical? Key/F k)
            [(text-editor/move-forwards self)]
            (identical? Key/B k)
            [(text-editor/move-backwards self)]
            (identical? Key/N k)
            [(text-editor/move-down self)]
            (identical? Key/P k)
            [(text-editor/move-up self)])))))

(extend-type TextEditor
  PTextEditor_Modes
  (insert-mode? [{:keys [state]}]
    (:insert-mode? @state))
  (enable-normal-mode [{:keys [state] :as self}]
    (swap! state (fn [state]
                   (-> state
                       (assoc :insert-mode? false)
                       (remove-key-handler :down :insert-mode)
                       (add-key-hanlder :down {:id :normal-mode
                                               :handler #(handle-keydown-normal-mode %1 %2)})
                       move/move-backwards))))
  (enable-insert-mode [{:keys [state]}]
    (swap! state (fn [state]
                   (-> state
                       (assoc :insert-mode? true)
                       (remove-key-handler :down :normal-mode)
                       (add-key-hanlder :down {:id :insert-mode
                                               :handler #(handle-keydown-insert-mode %1 %2)}))))))
