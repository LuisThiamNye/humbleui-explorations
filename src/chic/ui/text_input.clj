(ns chic.ui.text-input
  (:require
   [chic.focus :as focus]
   [io.github.humbleui.paint :as huipaint]
   [chic.key :as key]
   [chic.ui.event :as uievt]
   [chic.text-editor.line :as text-editor.line]
   [chic.ui :as cui]
   [chic.ui.focusable :as focusable]
   [chic.ui.layout :as cuilay]
   [chic.ui.subpixel :as cui.subpixel]
   [chic.ui.text :as cui.text]
   [chic.windows :as windows]
   [clojure.math :as math]
   [clojure.string :as str]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.ui :as ui]
   [taoensso.encore :as enc])
  (:import
   [io.github.humbleui.jwm EventKey Key KeyModifier MouseButton]
   [io.github.humbleui.skija Canvas Paint TextLine]
   [java.lang AutoCloseable]))

(deftype+ TextInput [get-text]
  IComponent
  (-measure [_ ctx cs])
  (-draw [_ ctx cs ^Canvas canvas])
  (-event [_ event])
  AutoCloseable
  (close [_]))

(defn cancel-cursor-animation [*cursor-state visible?]
  (some-> (:timeout @*cursor-state) future-cancel)
  (vswap! *cursor-state #(-> % (assoc :visible? visible?)
                             (assoc :last-updated 0)
                             (dissoc :timeout))))

(defn refresh-cursor-animation
  ([ctx *cursor-state visible?]
   (refresh-cursor-animation ctx *cursor-state visible? (System/currentTimeMillis)))
  ([ctx *cursor-state visible? t]
   (some-> (:timeout @*cursor-state) future-cancel)
   (let [to (enc/after-timeout
             (:cursor-blink-interval @*cursor-state)
             (windows/request-frame (:chic/current-window ctx)))]
     (vswap! *cursor-state #(-> % (assoc :visible? visible?)
                                (assoc :last-updated t)
                                (assoc :timeout to))))))

(defn key-down-event->action [{:keys [move-forwards
                                      move-backwards
                                      insert-str
                                      move-selection-forwards
                                      move-selection-backwards
                                      delete-backwards
                                      move-to-end
                                      move-to-start
                                      delete-to-end
                                      delete-to-start
                                      delete-forwards
                                      delete-backwards-word
                                      move-forwards-word
                                      move-backwards-word
                                      move-selection-backwards-word
                                      move-selection-forwards-word
                                      select-all
                                      move-selection-to-start
                                      move-selection-to-end]}
                              event]
  (let [ek ^EventKey (:eventkey event)
        k ^Key (.getKey ek)]
    (cond
      (key/no-modifiers? ek)
      (cond
        (identical? Key/RIGHT k)
        (move-forwards)
        (identical? Key/LEFT k)
        (move-backwards)
        #_#_#_#_(identical? Key/DOWN k)
              (move-down)
            (identical? Key/UP k)
          (move-up)
        (or (.isLetterKey k) (.isDigitKey k))
        (insert-str (str/lower-case (.getName k)))
        (identical? Key/SPACE k)
        (insert-str " ")
        ;; (identical? Key/TAB k)
        ;; (insert-str "\t")
        ;; (identical? Key/ENTER k)
        ;; (insert-str "\n")
        (identical? Key/BACKSPACE k)
        (delete-backwards 1)
        (identical? Key/SLASH k)
        (insert-str "/")
        (identical? Key/SEMICOLON k)
        (insert-str ";")
        (identical? Key/EQUALS k)
        (insert-str "=")
        (identical? Key/OPEN_BRACKET k)
        (insert-str "[")
        (identical? Key/BACK_SLASH k)
        (insert-str "\\")
        (identical? Key/CLOSE_BRACKET k)
        (insert-str "]")
        (identical? Key/COMMA k)
        (insert-str ",")
        (identical? Key/PERIOD k)
        (insert-str ".")
        (identical? Key/MINUS k)
        (insert-str "-")
        (identical? Key/MULTIPLY k)
        (insert-str "*")
        (identical? Key/ADD k)
        (insert-str "+")
        (identical? Key/SEPARATOR k)
        (insert-str ".")
        (identical? Key/DELETE k)
        (delete-forwards 1)
        (identical? Key/BACK_QUOTE k)
        (insert-str "`")
        (identical? Key/QUOTE k)
        (insert-str "'")
        (identical? Key/KANA k)
        (insert-str "wot")
        )
      (key/only-modifier? ek (key/mask KeyModifier/SHIFT))
      (cond
        (.isLetterKey k)
        (insert-str (.getName k))
        (.isDigitKey k)
        (insert-str (case (first (.getName k))
                      \1 "!"
                      \2 "@"
                      \3 "#"
                      \4 "$"
                      \5 "%"
                      \6 "^"
                      \7 "&"
                      \8 "*"
                      \9 "("
                      \0 ")"))
        (identical? Key/SPACE k)
        (insert-str " ")
        (identical? Key/SLASH k)
        (insert-str "?")
        (identical? Key/SEMICOLON k)
        (insert-str ":")
        (identical? Key/EQUALS k)
        (insert-str "+")
        (identical? Key/OPEN_BRACKET k)
        (insert-str "{")
        (identical? Key/BACK_SLASH k)
        (insert-str "|")
        (identical? Key/CLOSE_BRACKET k)
        (insert-str "}")
        (identical? Key/COMMA k)
        (insert-str "<")
        (identical? Key/PERIOD k)
        (insert-str ">")
        (identical? Key/MINUS k)
        (insert-str "-")
        (identical? Key/MULTIPLY k)
        (insert-str "*")
        (identical? Key/ADD k)
        (insert-str "+")
        (identical? Key/SEPARATOR k)
        (insert-str ".")
        (identical? Key/BACK_QUOTE k)
        (insert-str "~")
        (identical? Key/QUOTE k)
        (insert-str "\"")
        (identical? Key/RIGHT k)
        (move-selection-forwards)
        (identical? Key/LEFT k)
        (move-selection-backwards))
      (key/only-modifier? ek (key/mask KeyModifier/CONTROL))
      (cond
        (identical? Key/F k)
        (move-forwards)
        (identical? Key/B k)
        (move-backwards)
        #_#_#_#_(identical? Key/N k)
              (move-down)
            (identical? Key/P k)
          (move-up)
        (identical? Key/E k)
        (move-to-end)
        (identical? Key/A k)
        (move-to-start)
        (identical? Key/D k)
        (delete-forwards 1)
        (identical? Key/K k)
        (delete-to-end)
        (identical? Key/H k)
        (delete-backwards 1))
      (key/only-modifier? ek (or (key/mask KeyModifier/MAC_OPTION)
                                 (key/mask KeyModifier/ALT)))
      (cond
        (identical? Key/RIGHT k)
        (move-forwards-word)
        (identical? Key/LEFT k)
        (move-backwards-word)
        (identical? Key/BACKSPACE k)
        (delete-backwards-word))
      (key/only-modifier? ek (key/mask KeyModifier/MAC_COMMAND))
      (cond
        (identical? Key/A k)
        (select-all)
        (identical? Key/RIGHT k)
        (move-to-end)
        (identical? Key/LEFT k)
        (move-to-start)
        (identical? Key/BACKSPACE k)
        (delete-to-start))
      (key/only-modifier? ek (key/combine (or [KeyModifier/SHIFT KeyModifier/MAC_OPTION]
                                              [KeyModifier/SHIFT KeyModifier/ALT])))
      (cond
        (identical? Key/RIGHT k)
        (move-selection-forwards-word)
        (identical? Key/LEFT k)
        (move-selection-backwards-word))
      (key/only-modifier? ek (key/combine [KeyModifier/SHIFT KeyModifier/MAC_COMMAND]))
      (cond
        (identical? Key/RIGHT k)
        (move-selection-to-end)
        (identical? Key/LEFT k)
        (move-selection-to-start)))))

(defn clear-selection* [state]
  (assoc state :selection-idx nil))

(defn handle-key-down [{:keys [focus-manager] :as ctx} {:keys [*state focus-node] :as model} event]
  (let [move-forwards* (fn [{:keys [content cursor-idx] :as state}]
                         (assoc state :cursor-idx (min (inc cursor-idx) (count content))))
        move-forwards (fn [] (swap! *state (comp clear-selection* move-forwards*)))
        move-backwards* (fn [{:keys [cursor-idx] :as state}]
                          (assoc state :cursor-idx (max 0 (dec cursor-idx))))
        move-backwards (fn [] (swap! *state (comp clear-selection* move-backwards*)))
        move-to-end* (fn [{:keys [content] :as state}]
                       (assoc state :cursor-idx (count content)))
        move-to-end (fn [] (swap! *state (comp clear-selection* move-to-end*)))
        move-to-start* (fn [state] (assoc state :cursor-idx 0))
        move-to-start (fn [] (swap! *state (comp clear-selection* move-to-start*)))
        move-forwards-word* (fn [{:keys [cursor-idx] :as state}]
                              (if-let [idx (text-editor.line/next-end-of-word-idx state (dec cursor-idx))]
                                (assoc state :cursor-idx (inc idx))
                                state))
        move-forwards-word (fn [] (swap! *state (comp clear-selection* move-forwards-word*)))
        move-backwards-word* (fn [{:keys [cursor-idx] :as state}]
                               (if-let [idx (text-editor.line/prev-start-of-word-idx state cursor-idx)]
                                 (assoc state :cursor-idx idx)
                                 state))
        move-backwards-word (fn [] (swap! *state (comp clear-selection* move-backwards-word*)))
        delete-selection* (fn [{:keys [cursor-idx selection-idx content] :as state}]
                            (let [start-idx (min cursor-idx selection-idx)]
                              (-> state
                                  (assoc :content (str (subs content 0 start-idx)
                                                       (subs content (max cursor-idx selection-idx))))
                                  (assoc :cursor-idx start-idx)
                                  clear-selection*)))
        insert-str (fn [s]
                     (swap! *state
                            (fn [{:keys [selection-idx] :as state}]
                              (let [{:keys [content cursor-idx] :as state}
                                    (cond-> state selection-idx delete-selection*)]
                                (-> state
                                    (update :cursor-idx + (count s))
                                    (assoc :content (str (subs content 0 cursor-idx)
                                                         s
                                                         (subs content cursor-idx))))))))
        delete-between* (fn [{:keys [content] :as state} a b]
                          (assoc state :content (str (subs content 0 a) (subs content b))))
        delete-backwards (fn [n]
                           (swap! *state
                                  (fn [{:keys [content cursor-idx selection-idx] :as state}]
                                    (if selection-idx
                                      (delete-selection* state)
                                      (if (== 0 cursor-idx)
                                        state
                                        (-> state
                                            (update :cursor-idx dec)
                                            (delete-between* (- cursor-idx n) cursor-idx)))))))
        delete-forwards (fn [n]
                          (swap! *state
                                 (fn [{:keys [content cursor-idx selection-idx] :as state}]
                                   (if selection-idx
                                     (delete-selection* state)
                                     (if (>= cursor-idx (count content))
                                       state
                                       (delete-between* state cursor-idx (+ n cursor-idx)))))))
        delete-to-end (fn [] (swap! *state (fn [{:keys [selection-idx] :as state}]
                                             (if selection-idx
                                               (delete-selection* state)
                                               (delete-between* state (:cursor-idx state) (count (:content state)))))))
        delete-to-start (fn [] (swap! *state (fn [{:keys [selection-idx] :as state}]
                                               (if selection-idx
                                                 (delete-selection* state)
                                                 (-> state
                                                     (assoc :cursor-idx 0)
                                                     (delete-between* 0 (:cursor-idx state)))))))
        delete-backwards-word (fn [] (swap! *state (fn [{:keys [selection-idx cursor-idx] :as state}]
                                                     (if selection-idx
                                                       (delete-selection* state)
                                                       (if-let [idx (text-editor.line/prev-start-of-word-idx state cursor-idx)]
                                                         (-> state
                                                             (delete-between* idx cursor-idx)
                                                             (assoc :cursor-idx idx))
                                                         state)
                                                       ))))
        select-all (fn [] (swap! *state (fn [{:keys [content] :as state}]
                                          (-> state
                                              (assoc :selection-idx 0)
                                              (assoc :cursor-idx (count content))))))
        maybe-mark-selection-idx* (fn [{:keys [selection-idx] :as state}]
                                    (cond-> state (nil? selection-idx)
                                            (assoc :selection-idx (:cursor-idx state))))
        move-selection-forwards
        (fn []
          (swap! *state
                 (fn [state]
                   (-> state
                       maybe-mark-selection-idx*
                       move-forwards*))))
        move-selection-backwards
        (fn []
          (swap! *state
                 (fn [state]
                   (-> state
                       maybe-mark-selection-idx*
                       move-backwards*))))
        move-selection-forwards-word
        (fn []
          (swap! *state
                 (fn [state]
                   (-> state
                       maybe-mark-selection-idx*
                       move-forwards-word*))))
        move-selection-backwards-word
        (fn []
          (swap! *state
                 (fn [state]
                   (-> state
                       maybe-mark-selection-idx*
                       move-backwards-word*))))
        move-selection-to-end
        (fn []
          (swap! *state
                 (fn [state]
                   (-> state
                       maybe-mark-selection-idx*
                       move-to-end*))))
        move-selection-to-start
        (fn []
          (swap! *state
                 (fn [state]
                   (-> state
                       maybe-mark-selection-idx*
                       move-to-start*))))]
    (when (focus/has-focus? focus-manager focus-node)
      (refresh-cursor-animation ctx (:*cursor-state model) true)
      (key-down-event->action
       {:move-forwards move-forwards
        :move-backwards move-backwards
        :insert-str insert-str
        :move-selection-forwards move-selection-forwards
        :move-selection-backwards move-selection-backwards
        :move-to-end move-to-end
        :move-to-start move-to-start
        :move-forwards-word move-forwards-word
        :move-backwards-word move-backwards-word
        :delete-backwards delete-backwards
        :delete-to-end delete-to-end
        :delete-to-start delete-to-start
        :delete-forwards delete-forwards
        :delete-backwards-word delete-backwards-word
        :select-all select-all
        :move-selection-backwards-word move-selection-backwards-word
        :move-selection-forwards-word move-selection-forwards-word
        :move-selection-to-start move-selection-to-start
        :move-selection-to-end move-selection-to-end}
       event))))

(defn left-text-offset [config]
  (let [{:keys [left-padding label-subpixel-offset]} config]
    (+ left-padding label-subpixel-offset)))

(defn click-text-init [{:keys [*draw-state *state *cursor-state *config]} event]
  (let [idx (.getOffsetAtCoord
             ^TextLine (:textline @*draw-state)
             (:x (cui/component-relative-pos
                  event (update (:chic.ui/mouse-win-pos event)
                                :x - (left-text-offset @*config)))))]
    (cancel-cursor-animation *cursor-state true)
    (swap! *state
           (fn [state]
             (-> state
                 clear-selection*
                 (assoc :cursor-idx idx)
                 (assoc :mousing? true))))))

(defn handle-mouse-event [ctx {:keys [*state *draw-state *cursor-state] :as model} event]
  (when (and (not (:hui.event.mouse-button/is-pressed event))
             (identical? MouseButton/PRIMARY (uievt/mouse-button event)))
    (refresh-cursor-animation ctx *cursor-state true)
    (swap! *state (fn [{:keys [cursor-idx selection-idx] :as state}]
                    (-> state
                        (cond-> (= cursor-idx selection-idx)
                          clear-selection*)
                        (assoc :mousing? false))))))

(defn handle-textspan-click [model event]
  (when (and (:hui.event.mouse-button/is-pressed event)
             (identical? MouseButton/PRIMARY (uievt/mouse-button event)))
    (click-text-init model event)))

(defn make* []
  (let
   [*state (atom {:content "Hello wolrd"
                  :cursor-idx 0
                  :selection-idx nil})
    *config (volatile! {:left-padding 2
                        :label-subpixel-offset 0.5})
    *draw-state (volatile! {:offset 0
                            :textline nil})
    *cursor-state (volatile! {:visible? true
                              :last-updated 0
                              :cursor-blink-interval 700
                              :timeout nil})
    focus-node [::focus-node (random-uuid)]
    model {:*state *state
           :*cursor-state *cursor-state
           :*config *config
           :*draw-state *draw-state
           :focus-node focus-node}
    wrap-mousing-listener
    (fn [ui]
      (ui/dynamic _ [mousing? (:mousing? @*state)]
       (if mousing?
         (cui/on-mouse-move
          (fn [event]
            (swap! *state
                   (fn [state]
                     (let [idx (.getOffsetAtCoord ^TextLine (:textline @*draw-state)
                                                  (:x (cui/component-relative-pos
                                                       event
                                                       (update (:chic.ui/mouse-win-pos event)
                                                          :x - (left-text-offset @*config)))))]
                       (-> state
                          (cond-> (and (nil? (:selection-idx state))
                                       (not= idx (:cursor-idx state)))
                            (assoc :selection-idx (:cursor-idx state)))
                          (assoc :cursor-idx idx))))))
          ui)
         (cui/clickable
          (fn [event]
            (handle-textspan-click model event))
          ui))))
    ui (focusable/make
        {:focus-node focus-node}
        (ui/dynamic
         ctx [{:keys [fill-text focus-manager font-ui scale]
               window :chic/current-window} ctx]
         (let [ctx {:focus-manager focus-manager
                    :chic/current-window window}]
           (cui/clickable
            #(handle-mouse-event ctx model %)
            (ui/on-key-down
             #(handle-key-down ctx model %)
             (ui/dynamic
               ctx [state @*state
                    {:keys [left-padding label-subpixel-offset]} @*config]
               (let [textline ^TextLine (:textline (vswap! *draw-state assoc :textline
                                                 (cui.text/text-line (:content state) font-ui)))
                     textwidth (.getWidth textline)
                     right-padding 2
                     linewidth (Math/ceil (+ left-padding label-subpixel-offset textwidth right-padding))
                     ctx (assoc ctx :linewidth linewidth)]
                 (cui/clickable
                  (fn [event]
                    (when (:hui.event.mouse-button/is-pressed event)
                      (when (and (cui/point-in-component? event (:chic.ui/mouse-win-pos event))
                                 (let [p (cui/component-relative-pos event (:chic.ui/mouse-win-pos event))]
                                   (< (+ linewidth (:offset @*draw-state)) (:x p))))
                        (click-text-init model event))))
                  (cuilay/size-dependent
                   (fn [cs]
                     (let [excess (min (unchecked-subtract (Math/floorDiv (:width cs) scale) linewidth) 0)]
                       (cuilay/scrollable
                        (fn [event]
                          (vswap! *draw-state update :offset
                                  (fn [offset]
                                    (hui/clamp (unchecked-add offset (:hui.event.mouse-scroll/dx event))
                                               excess 0))))
                        (ui/dynamic
                          _ [{:keys [selection-idx cursor-idx prev-cursor-idx]} @*state]
                          (let [idx->x (fn [idx]
                                         (+ left-padding (math/round (/ (.getCoordAtOffset textline idx) scale))))
                                cursor-width 1
                                cursor-x (idx->x cursor-idx)
                                select-x (when selection-idx (idx->x selection-idx))
                                inner-ui
                                (wrap-mousing-listener
                                 (cuilay/stack
                                  (cuilay/padding
                                   0 4
                                   (cui.subpixel/row
                                    (cui.subpixel/gap (+ left-padding label-subpixel-offset) 0)
                                    (cui.subpixel/label-from-textline textline (:content state) font-ui fill-text)
                                    (cui.subpixel/gap right-padding 0)))
                                  ;; CURSOR
                                  (if (focus/has-focus? focus-manager focus-node)
                                    (cuilay/row
                                     (ui/gap cursor-x 0)
                                     (ui/dynamic _ [{:keys [visible? cursor-blink-interval]} @*cursor-state]
                                       (let [t (System/currentTimeMillis)]
                                         (when (and (not selection-idx)
                                                    (not (:mousing? @*state))
                                                    (>= (unchecked-subtract t (:last-updated @*cursor-state))
                                                        cursor-blink-interval))
                                           (refresh-cursor-animation ctx *cursor-state (not visible?) t))
                                         (ui/fill (if selection-idx
                                                    (huipaint/fill 0x70000000)
                                                    (if (:visible? @*cursor-state)
                                                      fill-text
                                                      (huipaint/fill 0x50000000)))
                                                  (ui/gap cursor-width 0))))
                                     [:stretch 1 (ui/gap 0 0)])
                                    (enc/do-nil
                                      (cancel-cursor-animation *cursor-state false)))
                                  ;; SELECTION
                                  (when select-x
                                    (let [start-x (min select-x cursor-x)
                                          end-x (max select-x cursor-x)]
                                      (cuilay/row
                                       (ui/gap (cond-> start-x
                                                 (< cursor-idx selection-idx)
                                                 (+ cursor-width)) 0)
                                       (ui/fill (doto (Paint.) (.setColor (unchecked-int 0x380000FF)))
                                                (ui/gap (unchecked-subtract-int end-x start-x) 0))
                                       [:stretch 1 (ui/gap 0 0)])))))]
                            (cui/on-draw
                             (fn [_ cs _]
                               ;; Make sure cursor is in view
                               (when-not (= cursor-idx prev-cursor-idx)
                                 (swap! *state assoc :prev-cursor-idx cursor-idx)
                                 (vswap! *draw-state update :offset
                                         (fn [offset]
                                           (hui/clamp offset (- left-padding cursor-x)
                                                      (min 0 (- (Math/floorDiv (:width cs) scale) cursor-x right-padding)))))))
                             (ui/dynamic
                               _ [offset (:offset @*draw-state)]
                               (cuilay/overflow-x offset inner-ui)))))))))))))))))]
    ui))

(defn make []
  (cui/dyncomp
   (make*)))

(comment
  (partition 2 (seq (.getPositions t)))
  (.getOffsetAtCoord t 20)
  #!
  )
