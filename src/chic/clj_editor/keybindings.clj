(ns chic.clj-editor.keybindings
  (:require
   [clojure.core.match :refer [match]]
   [chic.util :as util]
   [clojure.string :as str]
   [chic.ui :as cui]
   [chic.clj-editor.parser :as clj-editor.parser]
   [chic.clj-editor.nav :as clj-editor.nav]
   [chic.clj-editor.ast.string :as ast.string]
   [taoensso.encore :as enc]
   [clojure.core.match :refer [match]]
   [chic.clj-editor :as clj-editor]
   [chic.clj-editor.ast :as ast]
   [chic.key :as key])
  (:import
   (io.github.humbleui.jwm EventKey Key KeyModifier MouseButton Platform)
   (io.github.humbleui.skija Canvas Paint TextLine)
   (java.lang AutoCloseable)))

#_{:clj-kondo/ignore [:duplicate-case-test-constant]}
(def named-modifier-map
  (enc/case-eval (.ordinal Platform/CURRENT)
    (.ordinal Platform/WINDOWS)
    {:primary 'control
     :alt 'alt}
    (.ordinal Platform/MACOS)
    {:primary 'mac-command
     :alt 'mac-option}
    (.ordinal Platform/X11)
    {:primary 'control
     :alt 'alt}))

(defn keyspec->jwm-enum [k clsname]
  {:pre [(some? k)]}
  (let [s (str/replace
           (str/upper-case
            (name (cond-> k
                    (keyword? k)
                    (named-modifier-map))))
           #"-" "_")]
    (some (fn [jk]
            (when (= s (str jk)) jk))
          (.getEnumConstants (Class/forName clsname)))))

(defn keyspec->jwm [k]
  (keyspec->jwm-enum k (util/compile (.getName Key))))

(defn modspec->jwm [k]
  (keyspec->jwm-enum k (util/compile (.getName KeyModifier))))

(defn make-keybindings-handler [spec]
  (letfn [(merge-modspec [mask mods]
            (reduce (fn [mask ^KeyModifier km]
                      (+ mask (.-_mask km)))
                    mask
                    (eduction (map modspec->jwm)
                              (when mods
                                (if (coll? mods)
                                  mods
                                  [mods])))))
          (process [m entry mods]
            (match entry
              [:modifiers modspec & children]
              (reduce #(process %1 %2 (merge-modspec mods modspec))
                      m children)
              [:keys & kvs]
              (assoc m :keymap
                     (reduce
                      (fn [m [k effect]]
                        (update m mods assoc (keyspec->jwm k) effect))
                      (:keymap m)
                      (eduction (partition-all 2) kvs)))
              [:def effect child]
              (match child
                [:bindings & bindings]
                (reduce
                 (fn [m binding]
                   (let [modspec (butlast binding)
                         k (peek binding)]
                     (process m [:modifiers modspec
                                 [:keys k effect]]
                              mods)))
                 m bindings))
              [:remap & kvs]
              (assoc m :effectmap
                     (reduce
                      (fn [em [fromeffect effect]]
                        (assoc em fromeffect effect))
                      (:effectmap m)
                      (eduction (partition-all 2) kvs)))))]
    (let [bm (reduce (fn [m entry]
                       (process m entry 0))
                     {:keymap {}
                      :effectmap {}}
                     spec)
          bm (update bm :keymap
                     (fn [km]
                       (into {}
                             (map (fn [[mod km]]
                                    (let [em (java.util.EnumMap. Key)]
                                      (doseq [[k v] km]
                                        (.put em k v))
                                      [mod em])))
                             km)))]
      bm)))

(defn handle-keyevt [keymap modifiers ^Key k]
  (when-some [em ^java.util.Map (get keymap modifiers)]
    (.get em k)))

(defn get-binding* [handlers ^EventKey ek ^Key k]
  (let [remaps (java.util.ArrayList.)
        ignore-mask (cond->
                     (.-_mask KeyModifier/CAPS_LOCK)
                      (.isArrowKey (.getKey ek))
                      (bit-or (.-_mask KeyModifier/MAC_FN)))
        modifiers (bit-and-not (.-_modifiers ek) ignore-mask)]
    (some (fn [handler]
            (.add remaps (:effectmap handler))
            (when-some [effect (handle-keyevt (:keymap handler) modifiers k)]
              (loop [i (dec (.size remaps))
                     effect effect]
                (if (< i 0)
                  effect
                  (let [rm (.get remaps i)]
                    (recur (dec i) (get rm effect effect)))))))
          handlers)))

(defn get-binding [handlers event]
  (let [ek ^EventKey (:eventkey event)]
    (get-binding* handlers ek (.getKey ek))))

(def nav-keybindings
  [[:modifiers 'control
    [:keys
     'F :move-forwards
     'B :move-backwards
     'N :move-down
     'P :move-up
     'E :move-to-end
     'A :move-to-start
     'D :delete-forwards
     'K :kill
     'H :delete-backwards
     'W :delete-backwards-word]]
   [:modifiers :alt
    [:keys
     'right :move-forwards-word
     'left :move-backwards-word
     'backspace :delete-backwards-word]]
   [:keys
    'right :move-forwards
    'left :move-backwards
    'down :move-down
    'up :move-up
    'backspace :delete-backwards
    'delete :delete-forwards]
   [:modifiers 'shift
    [:keys
     'right :move-selection-forwards
     'left :move-selection-backwards]
    [:modifiers :alt
     [:keys
      'right :move-selection-forwards-word
      'left :move-selection-backwards-word]]
    [:modifiers :primary
     [:keys
      'right :move-selection-to-end
      'left :move-selection-to-start]]]
   [:modifiers :primary
    [:keys
     'A :select-all
     'right :move-to-end
     'left :move-to-start
     'backspace :delete-to-start
     'Z :undo]]])

(def nav-keybindings-handler (make-keybindings-handler nav-keybindings))

(def seg-mode-keybindings
  [[:keys
    'ENTER :insert-right
    'A :insert-right
    'I :insert-left]
   [:modifiers :primary
    [:keys
     'A :select-all-siblings]]
   [:remap
    :move-forwards :next-seg
    :move-backwards :prev-seg
    :move-down :down-seg
    :move-up :up-seg
    :move-to-end :last-sibling
    :move-to-start :first-sibling
    :delete-backwards :delete-seg
    :delete-forwards :delete-seg]])

(def seg-mode-keybindings-handler (make-keybindings-handler seg-mode-keybindings))

(def insert-mode-keybindings
  [[:def :seg-mode
    [:bindings
     ['escape]
     ['control 'open-bracket]]]
   [:remap
    :move-forwards :next
    :move-backwards :prev
    :move-down :down
    :move-up :up
    :delete-backwards :delete-prev
    :delete-forwards :delete-next]])

(def insert-mode-keybindings-handler (make-keybindings-handler insert-mode-keybindings))

(defn remap-default [cursor-pos effect]
  ({:next-seg [::clj-editor/move-to
               {:pos :next-seg}]
    :prev-seg [::clj-editor/move-to
               {:pos :prev-seg}]
    :up-seg [::clj-editor/move-to
             {:pos :up-seg}]
    :down-seg [::clj-editor/move-to
               {:pos :down-seg}]
    :next [::clj-editor/move-to
           {:pos :next}]
    :prev [::clj-editor/move-to
           {:pos :prev}]
    :up [::clj-editor/move-to
         {:pos :up-seg}]
    :down [::clj-editor/move-to
           {:pos :down-seg}]
    :seg-mode [::clj-editor/move-to
               {:pos (dissoc cursor-pos :local-idx)}]
    :insert-right
    [::clj-editor/move-to
     {:pos :insert-right}]
    :insert-left
    [::clj-editor/move-to
     {:pos (clj-editor.nav/assoc-pos-local-idx cursor-pos 0)}]
    :undo [::clj-editor/undo {}]} effect))

(defn handle-keydown-default [self ctx event]
  (let [state (::clj-editor/state ctx)
        cursor-pos (:cursor state)
        effect1 (get-binding [(if (:local-idx cursor-pos)
                                insert-mode-keybindings-handler
                                seg-mode-keybindings-handler)
                              nav-keybindings-handler]
                             event)]
    (when-some [effect (remap-default cursor-pos effect1)]
      [(update effect 1 assoc :state state)])))

(def ^java.util.HashMap node-type-keydown-handlers (java.util.HashMap.))

(defn emptyline-handle-keydown [self ctx event]
  (handle-keydown-default self ctx event))

(defn segment-default-keydown [self ctx event cursor-pos node]
  (handle-keydown-default self ctx event))

(defn simple-node-remap [state seg cursor-pos cmd]
  (case cmd
   :delete-seg
    [::clj-editor/ast.delete-simple-node
     {:node-id (ast/line-seg->node-id seg)
      :pos cursor-pos
      ::clj-editor/move-to
      {:state state
       :pos :prev-seg}}]
    nil))

(defn keybinding-handlers-default [cursor-pos]
  [(if (:local-idx cursor-pos)
     insert-mode-keybindings-handler
     seg-mode-keybindings-handler)
   nav-keybindings-handler])

(defn simple-segment-default-keydown [self ctx event cursor-pos node seg]
  (let [state (::clj-editor/state ctx)
        effect1 (get-binding (keybinding-handlers-default cursor-pos)
                             event)]
    (when-some [effect (or (simple-node-remap state seg cursor-pos effect1)
                           (remap-default cursor-pos effect1))]
      [(update effect 1 assoc :state state)])))

(defn handle-keydown [self ctx event]
  (let [state (::clj-editor/state ctx)
        ast (:ast state)
        cursor-pos (:cursor state)
        line-id (:line cursor-pos)
        line (get (::ast/lines ast) line-id)
        seg-idx (:seg-idx cursor-pos)
        effects (if-some [seg (when (and (seq line) seg-idx)
                                (nth line seg-idx))]
                  (let [node-id (ast/line-seg->node-id seg)
                        node (get (::ast/nodes ast) node-id)
                        seg-type (nth seg 0)]
                    (if-some [f (.get node-type-keydown-handlers (::ast/node-type node))]
                      (f self ctx event seg node ast cursor-pos (:local-idx cursor-pos))
                      (if (= ::ast/seg.simple seg-type)
                        (simple-segment-default-keydown self ctx event cursor-pos node seg)
                        (segment-default-keydown self ctx event cursor-pos node))))
                  (emptyline-handle-keydown self ctx event))]
    (when effects
      (cui/emit event effects))))

(defn handle-keyup [self ctx event])

(def ^java.util.HashMap text-handlers-by-type (java.util.HashMap.))

(defn emptyline-handle-text [self ctx text ast cursor-pos]
  )

(defn handle-text-input [self ctx event]
  (let [state (::clj-editor/state ctx)
        ast (:ast state)
        cursor (:cursor state)
        line-id (:line cursor)
        line (get (::ast/lines ast) line-id)
        seg-idx (:seg-idx cursor)
        text (:hui.event.text-input/text event)
        effects (if-some [seg (when (and (seq line) seg-idx)
                                (nth line seg-idx))]
                  (let [node-id (ast/line-seg->node-id seg)
                        node (get (::ast/nodes ast) node-id)]
                    (if-some [f (.get text-handlers-by-type (::ast/node-type node))]
                      (f self ctx text seg node ast cursor)
                      #_(handle-keydown-default self ctx event)))
                  (emptyline-handle-text self ctx text ast cursor))]
    (when effects (cui/emit event effects))))

(defn install-keydown-handler-for-node-type [typ f]
  (.put node-type-keydown-handlers typ f))

(defn install-text-handler-for-node-type [typ f]
  (.put text-handlers-by-type typ f))
