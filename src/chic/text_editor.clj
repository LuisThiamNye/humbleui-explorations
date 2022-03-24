(ns chic.text-editor
  (:require
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
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventMouseScroll
    EventKey Window Key KeyModifier]
   [io.github.humbleui.skija Canvas FontMgr FontStyle Typeface Font Paint PaintMode]
   [io.github.humbleui.types IPoint]))

(deftype+ TextEditor [^clojure.lang.Atom state ^:mut focus-node])

(defprotocol PTextEditor
  (move-forwards [_])
  (move-backwards [_])
  (element [_]))

(defprotocol PTextEditor_Modes
  (insert-mode? [_])
  (enable-normal-mode [_])
  (enable-insert-mode [_]))

(defprotocol PTextEditor_Insert
  (insert-str [_ s]))

(defprotocol PTextEditor_Delete
  (delete-up-to [_ p])
  (delete-between [_ p1 p2])
  (delete-backwards [_ n])
  (delete-forwards [_ n]))

(defprotocol PTextEditor_Pos
  (at-beginning? [_])
  (at-end? [_])
  (line-end-pos [_])
  (line-start-pos [_]))

(defn handle-keydown-default [self evt]
  (let [ek ^EventKey (:eventkey evt)
        k ^Key (.getKey ek)]
    (when (key/no-modifiers? ek)
      (cond
        (identical? Key/RIGHT k)
        [(move-forwards self)]
        (identical? Key/LEFT k)
        [(move-backwards self)]))))

(defn line-end-pos* [{:keys [content pos]}]
  (or (str/index-of content \newline pos)
      (unchecked-dec (count content))))

(defn line-start-pos* [{:keys [content pos]}]
  (if-some [i (str/last-index-of content \newline pos)]
    (inc i)
    0))

(defn next-end-of-big-word-pos* [{:keys [content pos]}]
  (let [m (re-matcher #"\S\s|$" content)]
    (.find m (inc pos))
    (.start m)))

(defn next-start-of-big-word-pos* [{:keys [content pos]}]
  (let [m (re-matcher #"\s+\S" content)]
    (.find m pos)
    (unchecked-dec-int (.end m))))

(defn prev-start-of-big-word-pos* [{:keys [content pos]}]
  (loop [i (unchecked-dec-int pos)
         seen-non-ws? false]
    (if (or (neg? i)
            (and seen-non-ws? (Character/isWhitespace (nth content i))))
      (inc i)
      (recur (unchecked-dec-int i)
             (or seen-non-ws?
                 (not (Character/isWhitespace (nth content i))))))))

(defn next-end-of-word-pos* [{:keys [content pos] :as sm}]
  (next-end-of-big-word-pos* sm))

(defn next-start-of-word-pos* [{:keys [content pos] :as sm}]
  (next-start-of-big-word-pos* sm))

(defn prev-start-of-word-pos* [{:keys [content pos] :as sm}]
  (prev-start-of-big-word-pos* sm))

(defn handle-keydown-normal-mode [self evt]
  (let [ek ^EventKey (:eventkey evt)
        k ^Key (.getKey ek)]
    (or
     (when (key/no-modifiers? ek)
       (cond
         (identical? Key/L k)
         [(move-forwards self)]
         (identical? Key/H k)
         [(move-backwards self)]
         (identical? Key/W k)
         [(swap! (:state self)
                 (fn [sm]
                   (assoc sm :pos (next-start-of-word-pos* sm))))]
         (identical? Key/B k)
         [(swap! (:state self)
                 (fn [sm]
                   (assoc sm :pos (prev-start-of-word-pos* sm))))]
         (identical? Key/E k)
         [(swap! (:state self)
                 (fn [sm]
                   (assoc sm :pos (next-end-of-word-pos* sm))))]
         (identical? Key/I k)
         [(enable-insert-mode self)]
         (identical? Key/X k)
         [(delete-forwards self 1)]
         (identical? Key/DIGIT0 k)
         [(swap! (:state self)
                 (fn [sm]
                   (assoc sm :pos (line-start-pos* sm))))]
         (identical? Key/BACKSPACE k)
         [(delete-backwards self 1)]))
     (when (key/only-modifier? ek (key/mask KeyModifier/SHIFT))
       (cond
         (identical? Key/W k)
         [(swap! (:state self)
                 (fn [sm]
                   (assoc sm :pos (next-start-of-big-word-pos* sm))))]
         (identical? Key/B k)
         [(swap! (:state self)
                 (fn [sm]
                   (assoc sm :pos (prev-start-of-big-word-pos* sm))))]
         (identical? Key/E k)
         [(swap! (:state self)
                 (fn [sm]
                   (assoc sm :pos (next-end-of-big-word-pos* sm))))]
         (identical? Key/DIGIT4 k)
         [(swap! (:state self)
                 (fn [sm]
                   (assoc sm :pos (line-end-pos* sm))))]
         (identical? Key/DIGIT6 k)
         [(swap! (:state self)
                 (fn [sm]
                   (assoc sm :pos (line-start-pos* sm))))])))))

(defn handle-keydown-insert-mode [self evt]
  (let [ek (:eventkey evt)
        k (.getKey ^EventKey ek)]
    (or (when (key/no-modifiers? ek)
          (cond
            (or (.isLetterKey k) (.isDigitKey k))
            (insert-str self (str/lower-case (.getName k)))
            (identical? Key/SPACE k)
            (insert-str self " ")
            (identical? Key/TAB k)
            (insert-str self "\t")
            (identical? Key/ENTER k)
            (insert-str self "\n")
            (identical? Key/BACKSPACE k)
            [(delete-backwards self 1)]
            (identical? Key/ESCAPE k)
            [(enable-normal-mode self)]))
        (when (key/only-modifier? ek (key/mask KeyModifier/SHIFT))
          (cond
            (.isLetterKey k)
            (insert-str self (.getName k))
            (.isDigitKey k)
            (insert-str self (case (first (.getName k))
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
            [(delete-forwards self 1)]
            (or (identical? Key/G k) (identical? Key/OPEN_BRACKET k))
            [(enable-normal-mode self)])))))

(defn editor-on-key-down [self focus-manager evt]
  (when (focus/has-focus? focus-manager (:focus-node self))
    (some #((:handler %) self evt)
          (reverse (:keydown-handlers @(:state self))))))

(defn make [opts]
  (let [e (->TextEditor
           (atom {:pos (:pos opts 0)
                  :content (:content opts "")
                  :keydown-handlers [{:id "default"
                                      :handler #(handle-keydown-default %1 %2)}]
                  :face-default (:face-default opts)})
           [::focus-node (random-uuid)])]
    (enable-normal-mode e)
    e))

(extend-type TextEditor
  PTextEditor
  (move-forwards [{:keys [state] :as self}]
    (when-not (at-end? self)
      (swap! state update :pos inc)))
  (move-backwards [{:keys [state] :as self}]
    (when-not (at-beginning? self)
      (swap! state update :pos dec)))
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
            {:keys [content pos]} state'
            leading (-> font-code .getMetrics .getCapHeight Math/ceil (/ scale))
            fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
            vpadding (/ leading 2)]
        (ui/on-key-down
         #(editor-on-key-down self focus-manager %)
         (ui/row
          (ui/padding
           0 vpadding
           (ui/label (subs content 0 pos) font-code fill-text))
          (if (insert-mode? self)
            (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFF4070D0)))
                     (ui/padding
                      0 vpadding
                      (ui/label (str (nth content pos \space))
                                font-code (doto (Paint.) (.setColor (unchecked-int 0xFFFFFFFF))))))
            (ui/clip-rrect
             2
             (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFD07040)))
                      (ui/padding
                       0 vpadding
                       (ui/label (str (nth content pos \space))
                                 font-code (doto (Paint.) (.setColor (unchecked-int 0xFFFFFFFF))))))))
          (ui/padding
           0 vpadding
           (ui/label (subs content (min (inc pos) (count content))) font-code fill-text)))))))))

(defn remove-key-handler [state up-or-down id]
  (update state (case up-or-down :down :keydown-handlers)
          (fn [handlers]
            (filterv #(not= id (:id %)) handlers))))

(defn add-key-hanlder [state up-or-down handler]
  (update state (case up-or-down :down :keydown-handlers)
          conj handler))

(extend-type TextEditor
  PTextEditor_Modes
  (insert-mode? [{:keys [state]}]
    (:insert-mode? @state))
  (enable-normal-mode [{:keys [state] :as self}]
    (when (at-end? self)
      (move-backwards self))
    (swap! state (fn [state]
                   (-> state
                       (assoc :insert-mode? false)
                       (remove-key-handler :down :insert-mode)
                       (add-key-hanlder :down {:id :normal-mode
                                               :handler #(handle-keydown-normal-mode %1 %2)})))))
  (enable-insert-mode [{:keys [state]}]
    (swap! state (fn [state]
                   (-> state
                       (assoc :insert-mode? true)
                       (remove-key-handler :down :normal-mode)
                       (add-key-hanlder :down {:id :insert-mode
                                               :handler #(handle-keydown-insert-mode %1 %2)}))))))

(extend-type TextEditor
  PTextEditor_Insert
  (insert-str [{:keys [state]} s]
    (swap! state (fn [{:keys [content pos] :as state}]
                   (-> state
                       (assoc :content
                           (str (subs content 0 pos)
                                s
                                (subs content pos)))
                       (update :pos + (count s)))))))

(defn delete-up-to* [{:keys [content pos] :as state} p]
  (if (< p pos)
    (-> state
        (assoc :content
               (str (subs content 0 p)
                    (subs content pos)))
        (assoc :pos p))
    (-> state
        (assoc :content
               (str (subs content 0 pos)
                    (subs content p))))))

(extend-type TextEditor
  PTextEditor_Delete
  (delete-up-to [{:keys [state]} p]
    (swap! state delete-up-to* p))
  (delete-backwards [{:keys [state]} n]
    (swap! state (fn [{:keys [content pos] :as state}]
                   (delete-up-to* state (unchecked-subtract-int pos n)))))
  (delete-forwards [{:keys [state]} n]
    (swap! state (fn [{:keys [content pos] :as state}]
                   (delete-up-to* state (+ n pos))))))

(extend-type TextEditor
  PTextEditor_Pos
  (at-beginning? [{:keys [state]}]
    (zero? (:pos @state)))
  (at-end? [{:keys [state] :as self}]
    (let [state' @state]
      (== (:pos state') (cond-> (count (:content state'))
                          (not (insert-mode? self))
                          unchecked-dec))))
  (line-start-pos [{:keys [state]}]
    (line-start-pos* state))
  (line-end-pos [{:keys [state]}]
    (line-end-pos* state)))
