(ns chic.clj-editor.keybindings
  (:require
   [chic.ui :as cui]
   [chic.clj-editor.parser :as clj-editor.parser]
   [chic.clj-editor.nav :as clj-editor.nav]
   [chic.clj-editor.ast.string :as ast.string]
   [clojure.core.match :refer [match]]
   [chic.clj-editor :as clj-editor]
   [chic.clj-editor.ast :as ast]
   [chic.key :as key])
  (:import
   (io.github.humbleui.jwm EventKey Key KeyModifier MouseButton)
   (io.github.humbleui.skija Canvas Paint TextLine)
   (java.lang AutoCloseable)))

(defn keydown->insert-nav-effect [^EventKey ek ^Key k]
  (cond
    (key/no-modifiers? ek)
    (cond
      (identical? Key/RIGHT k)
      [::move-forwards]
      (identical? Key/LEFT k)
      [::move-backwards]
      (identical? Key/DOWN k)
      [::move-down]
      (identical? Key/UP k)
      [::move-up]
      (identical? Key/BACKSPACE k)
      [::delete-backwards]
      (identical? Key/DELETE k)
      [::delete-forwards])
    (key/only-modifier? ek (key/mask KeyModifier/SHIFT))
    (cond
      (identical? Key/RIGHT k)
      [::move-selection-forwards]
      (identical? Key/LEFT k)
      [::move-selection-backwards])
    (key/only-modifier? ek (key/mask KeyModifier/CONTROL))
    (cond
      (identical? Key/F k)
      [::move-forwards]
      (identical? Key/B k)
      [::move-backwards]
      (identical? Key/N k)
      [::move-down]
      (identical? Key/P k)
      [::move-up]
      (identical? Key/E k)
      [::move-to-end]
      (identical? Key/A k)
      [::move-to-start]
      (identical? Key/D k)
      [::delete-forwards]
      (identical? Key/K k)
      [::kill]
      (identical? Key/H k)
      [::delete-backwards]
      (identical? Key/W k)
      [::delete-backwards-word])
    (key/only-modifier? ek (or (key/mask KeyModifier/MAC_OPTION)
                               (key/mask KeyModifier/ALT)))
    (cond
      (identical? Key/RIGHT k)
      [::move-forwards-word]
      (identical? Key/LEFT k)
      [::move-backwards-word]
      (identical? Key/BACKSPACE k)
      [::delete-backwards-word])
    (key/only-modifier? ek (key/mask KeyModifier/MAC_COMMAND))
    (cond
      (identical? Key/A k)
      [::select-all]
      (identical? Key/RIGHT k)
      [::move-to-end]
      (identical? Key/LEFT k)
      [::move-to-start]
      (identical? Key/BACKSPACE k)
      [::delete-to-start])
    (key/only-modifier? ek (key/combine (or [KeyModifier/SHIFT KeyModifier/MAC_OPTION]
                                            [KeyModifier/SHIFT KeyModifier/ALT])))
    (cond
      (identical? Key/RIGHT k)
      [::move-selection-forwards-word]
      (identical? Key/LEFT k)
      [::move-selection-backwards-word])
    (key/only-modifier? ek (key/combine [KeyModifier/SHIFT KeyModifier/MAC_COMMAND]))
    (cond
      (identical? Key/RIGHT k)
      [::move-selection-to-end]
      (identical? Key/LEFT k)
      [::move-selection-to-start])))

[::delete-region 'idx1 'idx2]
[::move-to 'idx2]
[::select-region 'idx1 'idx2]
:line-start
:doc-start
:line-end
:doc-end
:next-word
:prev-word
:next
:prev
:next-seg
:prev-seg

(defn handle-keydown-default [self ctx event]
  (let [state (::clj-editor/state ctx)
        ek ^EventKey (:eventkey event)
        k ^Key (.getKey ek)
        [effect1] (keydown->insert-nav-effect ek k)]
    (or (when-some [effect (get {::move-forwards [::clj-editor/move-to
                                               {:pos :next-seg
                                                :state state}]
                              ::move-backwards [::clj-editor/move-to
                                                {:pos :prev-seg
                                                 :state state}]
                              ::move-up [::clj-editor/move-to
                                         {:pos :up-seg
                                          :state state}]
                              ::move-down [::clj-editor/move-to
                                           {:pos :down-seg
                                            :state state}]}
                             effect1)]
          (cui/emit ctx [effect]))
        (when (key/only-modifier? ek (key/mask KeyModifier/MAC_COMMAND))
          (cond (identical? Key/Z k)
                (cui/emit ctx [[::clj-editor/undo {}]]))))))

(def ^java.util.HashMap node-type-keydown-handlers (java.util.HashMap.))

(defn handle-keydown [self ctx event]
  (let [state (::clj-editor/state ctx)
        ast (:ast state)
        cursor (:cursor state)
        line-id (:line cursor)
        line (get (::ast/lines ast) line-id)
        seg-idx (:seg-idx cursor)]
    (if-some [seg (when (and (seq line) seg-idx)
                    (nth line seg-idx))]
      (let [node-id (ast/line-seg->node-id seg)
            node (get (::ast/nodes ast) node-id)]
        (if-some [f (.get node-type-keydown-handlers (::ast/node-type node))]
          (f self ctx event seg node ast cursor (:local-idx cursor))
          (handle-keydown-default self ctx event)))
      ;; Empty line
      (handle-keydown-default self ctx event))))

(defn handle-keyup [self ctx event])

(def ^java.util.HashMap text-handlers-by-type (java.util.HashMap.))

(defn handle-text-input [self ctx event]
  (let [state (::clj-editor/state ctx)
        ast (:ast state)
        cursor (:cursor state)
        line-id (:line cursor)
        line (get (::ast/lines ast) line-id)
        seg-idx (:seg-idx cursor)]
    (if-some [seg (when (and (seq line) seg-idx)
                    (nth line seg-idx))]
      (let [node-id (ast/line-seg->node-id seg)
            node (get (::ast/nodes ast) node-id)]
        (if-some [f (.get text-handlers-by-type (::ast/node-type node))]
          (f self ctx (:hui.event.text-input/text event) seg node ast cursor (:local-idx cursor))
          #_(handle-keydown-default self ctx event)))
      ;; Empty line
      #_(handle-keydown-default self ctx event))))

(defn symbol-valid-as-transient? [symstr]
  (some? (re-matches #"[!-~\u0080-\uFFFF]+" symstr)))

(defn parse-generous-symbol [symstr]
  (let [[_ a b] (re-matches #"(?:([^/]*)/)?(.*)" symstr)]
    (when b (symbol a b))))

(.put node-type-keydown-handlers
      ::ast/type.symbol
      (fn keydown [self ctx event seg node ast cursor-pos cursor-idx]
        (or (let [ek ^EventKey (:eventkey event)
                  k ^Key (.getKey ek)
                  state (::clj-editor/state ctx)
                  [effect1] (keydown->insert-nav-effect ek k)
                  nstr (ast.string/simple-node->string node)
                  escape-eft [::clj-editor/move-to
                              {:pos (dissoc cursor-pos :local-idx)
                               :state state}]]
              (if (nil? cursor-idx)
                (when (key/no-modifiers? ek)
                  (cond
                    (or (identical? Key/ENTER k)
                        (identical? Key/A k))
                    (cui/emit ctx [[::clj-editor/move-to
                                    {:pos :insert-right
                                     :state state}]])
                    (identical? Key/I k)
                    (cui/emit ctx [[::clj-editor/move-to
                                    {:pos :insert-left
                                     :state state}]])))
                (if-some [effect (get {::move-forwards
                                       [::clj-editor/move-to
                                        {:pos (if (< cursor-idx (count nstr))
                                                (clj-editor.nav/assoc-pos-local-idx
                                                 cursor-pos (inc cursor-idx))
                                                :next-seg)
                                         :state state}]
                                       ::move-backwards
                                       [::clj-editor/move-to
                                        {:pos (if (< 0 cursor-idx)
                                                (clj-editor.nav/assoc-pos-local-idx
                                                 cursor-pos (dec cursor-idx))
                                                :prev-seg)
                                         :state state}]
                                       ::move-up [::clj-editor/move-to
                                                  {:pos :up-seg
                                                   :state state}]
                                       ::move-down [::clj-editor/move-to
                                                    {:pos :down-seg
                                                     :state state}]
                                       ::delete-backwards
                                       [::clj-editor/delete-region
                                        {:start-pos :prev
                                         :end-pos cursor-pos
                                         :state state}]}
                                      effect1)]
                  (cui/emit
                   ctx
                   (case (nth effect 0)
                     ::clj-editor/delete-region
                     (let [{:keys [state] :as info} (nth effect 1)
                           ast (:ast state)
                           cursor (:cursor state)]
                       (when-some [start-pos (clj-editor.nav/resolve-pos
                                              self ast cursor (:start-pos info))]
                         (when-some [end-pos (clj-editor.nav/resolve-pos
                                              self ast cursor (:end-pos info))]
                           (when (and (= (:seg-idx start-pos) (:seg-idx end-pos))
                                      (= (:line start-pos) (:line end-pos)))
                             (let [start-idx (:local-idx start-pos)
                                   end-idx (:local-idx end-pos)
                                   namsp (::ast/symbol.ns node)
                                   namsp-cnt (count namsp)
                                   nam (::ast/symbol.name node)
                                   new-ns? (< start-idx namsp-cnt)
                                   new-name? (< namsp-cnt end-idx)
                                   ns2 (if new-ns?
                                         (str (subs namsp 0 start-idx)
                                              (subs namsp (min namsp-cnt end-idx)))
                                         namsp)
                                   blank-ns? (<= (count ns2) 1)
                                   ns2 (if blank-ns? nil ns2)
                                   merge? (and new-ns?
                                               (or (<= namsp-cnt end-idx) blank-ns?))
                                   new-name? (or new-name? merge?)
                                   nam2 (if new-name?
                                          (let [nam2 (str (subs nam 0 (max 0 (- start-idx namsp-cnt)))
                                                          (subs nam (max 0 (- end-idx namsp-cnt))))]
                                            (if merge?
                                              (str ns2 nam2)
                                              nam2))
                                          nam)
                                   ns2 (when-not (or merge? (== 0 (count ns2)))
                                         ns2)]
                               (when (or new-name? new-ns?)
                                 (if (and (nil? ns2) (== 0 (count nam2)))
                                  [[::clj-editor/ast.delete-simple-node
                                    {:node-id (ast/line-seg->node-id seg)
                                     :pos cursor-pos}]
                                   [::clj-editor/move-to
                                    {:pos :prev-seg
                                     :state state}]]
                                  (let [symstr (str ns2 nam2)
                                        transient? (and (nil? (clj-editor.parser/parse-symbol symstr))
                                                        (symbol-valid-as-transient? symstr))]
                                    (cond->
                                     [[::clj-editor/ast.update-node
                                       {:node-id (ast/line-seg->node-id seg)
                                        :new-node
                                        (-> (dissoc node ::ast/transient-node?)
                                            (cond-> transient?
                                              (assoc ::ast/transient-node? transient?))
                                            (cond-> new-ns?
                                              (assoc ::ast/symbol.ns ns2))
                                            (cond-> new-name?
                                              (assoc ::ast/symbol.name nam2)))}]]
                                     (< start-idx cursor-idx)
                                     (conj [::clj-editor/move-to
                                            {:pos (clj-editor.nav/assoc-pos-local-idx
                                                   cursor-pos (max start-idx (- cursor-idx (- end-idx start-idx))))
                                             :state state}]))))))))))
                     [effect]))
                  (or (when (key/no-modifiers? ek)
                        (cond (identical? Key/ESCAPE k)
                              (cui/emit ctx [escape-eft])))
                      (when (key/only-modifier? ek (key/mask KeyModifier/CONTROL))
                        (cond
                          (identical? Key/OPEN_BRACKET k)
                          (cui/emit ctx [escape-eft])))))))
            (handle-keydown-default self ctx event))))

(.put text-handlers-by-type
      ::ast/type.symbol
      (fn[self ctx text seg node ast cursor-pos cursor-idx]
        (when-not (nil? cursor-idx)
          (let [repr (ast.string/simple-node->string node)
               new-repr (str (subs repr 0 cursor-idx)
                             text
                             (subs repr cursor-idx))
                sym (clj-editor.parser/parse-symbol new-repr)
                transient? (and (nil? sym)
                                (symbol-valid-as-transient? new-repr))
                sym (if transient?
                      (parse-generous-symbol new-repr)
                      sym)]
            (when (or sym transient?)
             (cui/emit
              ctx
              [[::clj-editor/ast.update-node
                {:node-id (ast/line-seg->node-id seg)
                 :new-node
                 (-> (dissoc node ::ast/transient-node?)
                     (assoc ::ast/transient-node? transient?)
                     (assoc ::ast/symbol.ns (some-> (namespace sym) (str "/")))
                     (assoc ::ast/symbol.name (name sym)))}]
               [::clj-editor/move-to
                {:pos (clj-editor.nav/assoc-pos-local-idx
                       cursor-pos (+ cursor-idx (count text)))
                 :state (::clj-editor/state ctx)}]]))))))

#_(effect-handler
   ::ast/type.comment
   (match
    [::delete-region idx1 idx2]

     [:atom
      [::uncomment]
      [::move-to]])
   (fn text-input [idx text]
     (or (when (and at-prefix? prefix-before?)
           [::invalid])
         [[::ensure-whitespace]
          effect]))
   (fn keydown [idx ek]
     (or
      (when prefix-before?
        (cond
          {::delete-backwards [::uncomment :cursor-to-start]
           ::delete-to-start nil}))
      (when at-prefix?
        (cond
          {::delete-forwards [::uncomment :cursor-to-start]
           ::kill [[::copy-text from-cursor-to-end]
                   [::delete-node nid]]})))))
