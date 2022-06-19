(ns chic.clj-editor.seg
  (:require
   [clojure.core.match :refer [match]]
   [chic.clj-editor.keybindings :as keybindings]
   [chic.clj-editor.ast :as ast]
   [chic.clj-editor.parser :as parser]
   [chic.clj-editor.nav :as clj-editor.nav]
   [chic.clj-editor.ast.string :as ast.string]
   [chic.clj-editor :as clj-editor]))

(defn str-insert-at [s idx text]
  (str (subs s 0 idx) text (subs s idx)))

(defn str-delete-between [s a b]
  (str (subs s 0 a) (subs s b)))

(defn parse-to-node [ctor fit-f transient-f s]
  (if-some [fit (fit-f s)]
    (ctor fit)
    (when-some [t (transient-f s)]
      (assoc (ctor t) ::ast/transient-node? true))))

(defn parse-generous-symbol [symstr]
  (when (re-matches #"[!-~\u0080-\uFFFF]+" symstr)
    (let [[_ a b] (re-matches #"(?:([^/]*)/)?(.*)" symstr)]
      (when b (symbol a b)))))

(defn parse-transient-keyword-symbol [symstr]
  (or (when (== 0 (count symstr)) (symbol nil symstr))
      (parse-generous-symbol symstr)))

#_(.put node-type-keydown-handlers
        ::ast/type.keyword
        (fn keydown [self ctx event seg node ast cursor-pos cursor-idx]))
(defn handle-delete-region-local [self state start-pos end-pos f]
  (let [ast (:ast state)
        cursor (:cursor state)]
    (when-some [start-pos (clj-editor.nav/resolve-pos
                           self ast cursor start-pos)]
      (when-some [end-pos (clj-editor.nav/resolve-pos
                           self ast cursor end-pos)]
        (when (and (= (:seg-idx start-pos) (:seg-idx end-pos))
                   (= (:line start-pos) (:line end-pos)))
          (f (:local-idx start-pos) (:local-idx end-pos)))))))

#_(defn symbol-handle-delete-region [state seg node cursor-pos start-idx end-idx]
    (let [cursor-idx (:local-idx cursor-pos)
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
          [::clj-editor/ast.delete-simple-node
           {:node-id (ast/line-seg->node-id seg)
            :pos cursor-pos
            ::clj-editor/move-to
            {:pos :prev-seg
             :state state}}]
          (let [symstr (str ns2 nam2)
                good-sym (parser/parse-symbol symstr)
                transient? (and (nil? good-sym)
                                (symbol-valid-as-transient? symstr))
                sym (or good-sym (parse-generous-symbol symstr))
                ns2 (some-> sym namespace (str "/"))
                nam2 (name sym)]
            [::clj-editor/ast.update-node
             {:node-id (ast/line-seg->node-id seg)
              :new-node
              (-> (dissoc node ::ast/transient-node?)
                  (cond-> transient?
                    (assoc ::ast/transient-node? transient?))
                  (cond-> new-ns?
                    (assoc ::ast/symbol.ns ns2))
                  (cond-> new-name?
                    (assoc ::ast/symbol.name nam2)))
              ::clj-editor/move-to
              (when (< start-idx cursor-idx)
                {:pos (clj-editor.nav/assoc-pos-local-idx
                       cursor-pos (max start-idx (- cursor-idx (- end-idx start-idx))))
                 :state state})}])))))
(defn simple-adj-node [typ _old repr]
  (when-not (== 0 (count repr))
    (or (when (parser/constant? repr)
          (ast/constant-node (read-string repr)))
        (case (nth repr 0)
          \: (when-not (= ::ast/type.keyword typ)
               (if (re-find #"^::" repr)
                 (if (re-find #"/" repr)
                   (parse-to-node ast/keyword-alias-node
                                  parser/parse-symbol
                                  parse-transient-keyword-symbol
                                  (subs repr 2))
                   (parse-to-node ast/keyword-auto-node
                                  parser/parse-symbol
                                  parse-transient-keyword-symbol
                                  (subs repr 2)))
                 (parse-to-node ast/keyword-node
                                parser/parse-symbol
                                parse-transient-keyword-symbol
                                (subs repr 1))))
          \% (when-not (= ::ast/type.arg typ)
               (parse-to-node
                ast/arg-node
                #(when-some [[_ n] (re-matches #"%([1-9&]?)" %)]
                   n)
                #(subs % 1)
                repr))
          \# (or (when-not (= ::ast/type.var typ)
                   (when-some [[_ sym] (re-matches #"#'(\S*)" repr)]
                     (parse-to-node ast/var-node
                                    parser/parse-symbol
                                    parse-generous-symbol
                                    sym)))
                 (when-not (= ::ast/type.symbolic-val typ)
                   (parse-to-node ast/symbolic-val-node
                                  #(some-> (re-matches #"##(Inf|-Inf|NaN)" %)
                                           (nth 1))
                                  #(some-> (re-matches #"##(\S*)" %)
                                           (nth 1))
                                  repr)))
          \\ (when-not (= ::ast/type.char typ)
               (parse-to-node ast/char-node
                              #(when (char? (try (read-string %)
                                                 (catch Throwable _)))
                                 (some-> (re-matches #"\\( |\S+)" %)
                                         (nth 1)))
                              #(some-> (re-matches #"\\( |\S+)" %)
                                       (nth 1))
                              repr))
          nil)
        (when-not (= ::ast/type.number typ)
          (parse-to-node ast/number-node
                         #(when (parser/match-number %) %)
                         #(re-matches #"(?:\+|-)?\d.*" %)
                         repr))
        (when-not (= ::ast/type.symbol typ)
          (parse-to-node ast/symbol-node parser/parse-symbol parse-generous-symbol repr)))))

(defn effect-update-node [seg node transient? move-to-info]
  [::clj-editor/ast.update-node
   {:node-id (ast/line-seg->node-id seg)
    :new-node
    (-> (dissoc node ::ast/transient-node?)
        (assoc ::ast/transient-node? (boolean transient?)))
    ::clj-editor/move-to move-to-info}])

(defn effect-adj-replace-simple-node [typ state seg cursor-pos old-repr new-repr move-to-info]
  (or (when-some [adjacent (simple-adj-node typ old-repr new-repr)]
        [::clj-editor/ast.replace-simple-node
         {:node-id (ast/line-seg->node-id seg)
          :pos cursor-pos
          :new-node adjacent
          ::clj-editor/move-to move-to-info}])
      (when (== 0 (count new-repr))
        [::clj-editor/ast.delete-simple-node
         {:node-id (ast/line-seg->node-id seg)
          :pos cursor-pos
          ::clj-editor/move-to
          {:state state
           :pos :prev-seg}}])))

(defn move-to-info-local-idx [state cursor-pos idx]
  {:pos (clj-editor.nav/assoc-pos-local-idx cursor-pos idx)
   :state state})

(defn simple-node-effect-default [self state seg node cursor-pos cmd]
  (let [cursor-idx (:local-idx cursor-pos)]
    (case cmd
      :delete-prev
      (when (< 0 cursor-idx)
        (let [old-repr (ast.string/simple-node->string node)
              new-repr (str-delete-between old-repr (dec cursor-idx) cursor-idx)]
          [::change-text {:from old-repr :to new-repr :move-to-idx (dec cursor-idx)}]))
      :delete-next
      (let [old-repr (ast.string/simple-node->string node)]
        (when (< cursor-idx (count old-repr))
          (let [new-repr (str-delete-between old-repr cursor-idx (inc cursor-idx))]
            [::change-text {:from old-repr :to new-repr :move-to-idx nil}])))
      ;; :delete-prev
      ;; (handle-delete-region-local
      ;;  self state :prev cursor-pos
      ;;  (partial symbol-handle-delete-region
      ;;           state seg node cursor-pos))
      ;; :delete-next
      ;; (handle-delete-region-local
      ;;  self state cursor-pos :next
      ;;  (partial symbol-handle-delete-region
      ;; state seg node cursor-pos))
      nil)))

(defn symbol-handle-changed-text [state seg node cursor-pos old-repr new-repr idx2]
  (let [sym (parser/parse-symbol new-repr)
        tsym (and (nil? sym) (parse-generous-symbol new-repr))
        move-to-info (when idx2 (move-to-info-local-idx state cursor-pos idx2))
        update-node
        #(-> node
             (assoc ::ast/symbol.ns (some-> (namespace %) (str "/")))
             (assoc ::ast/symbol.name (name %)))]
    (cond
      sym
      (effect-update-node seg (update-node sym) false move-to-info)
      :else
      (or (effect-adj-replace-simple-node
           ::ast/type.symbol state seg cursor-pos old-repr new-repr move-to-info)
          (when tsym
            (effect-update-node seg (update-node tsym) true move-to-info))))))

(defn simple-node-keydown-effect-default [self state seg node event cursor-pos]
  (let [effect (keybindings/get-binding (keybindings/keybinding-handlers-default cursor-pos)
                                        event)]
    (when-some [effect (or (simple-node-effect-default self state seg node cursor-pos effect)
                           (keybindings/simple-node-remap state seg cursor-pos effect)
                           (keybindings/remap-default cursor-pos effect))]
      (update effect 1 assoc :state state))))

(keybindings/install-keydown-handler-for-node-type
 ::ast/type.symbol
 (fn keydown [self ctx event seg node ast cursor-pos cursor-idx]
   (let [state (::clj-editor/state ctx)
         effect (simple-node-keydown-effect-default
                 self state seg node event cursor-pos)
         effect (match effect
                  [::change-text {:from old-repr :to new-repr :move-to-idx toidx}]
                  (symbol-handle-changed-text state seg node cursor-pos old-repr new-repr toidx)
                  :else effect)]
     (some-> effect vector))))

(keybindings/install-text-handler-for-node-type
 ::ast/type.symbol
 (fn [self ctx text seg node ast cursor-pos]
   (let [cursor-idx (:local-idx cursor-pos)]
     (when-not (nil? cursor-idx)
       (let [repr (ast.string/simple-node->string node)
             new-repr (str-insert-at repr cursor-idx text)
             r (symbol-handle-changed-text
                (::clj-editor/state ctx) seg node cursor-pos repr new-repr
                (+ cursor-idx (count text)))]
         (some-> r vector))))))

(defn keyword-handle-changed-text [state seg node cursor-pos old-repr new-repr idx2]
  (let [move-to-info (when idx2 (move-to-info-local-idx state cursor-pos idx2))]
    (if (re-find #"^:" new-repr)
      (let [auto? (re-find #"^::" new-repr)
            rawsym (subs new-repr (if auto? 2 1))
            sym (parser/parse-symbol rawsym)
            tsym (and (nil? sym) (parse-transient-keyword-symbol rawsym))
            update-node
            #(-> node
                 (assoc ::ast/keyword.auto? auto?)
                 (assoc ::ast/symbol.ns (some-> (namespace %) (str "/")))
                 (assoc ::ast/symbol.name (name %)))]
        (cond
          sym (effect-update-node seg (update-node sym) false move-to-info)
          tsym (effect-update-node seg (update-node tsym) true move-to-info)))
      (effect-adj-replace-simple-node
       ::ast/type.keyword state seg cursor-pos old-repr new-repr move-to-info))))

(keybindings/install-keydown-handler-for-node-type
 ::ast/type.keyword
 (fn keydown [self ctx event seg node ast cursor-pos cursor-idx]
   (let [state (::clj-editor/state ctx)
         effect (simple-node-keydown-effect-default
                 self state seg node event cursor-pos)
         effect (match effect
                  [::change-text {:from old-repr :to new-repr :move-to-idx toidx}]
                  (keyword-handle-changed-text state seg node cursor-pos old-repr new-repr toidx)
                  :else effect)]
     (some-> effect vector))))

(keybindings/install-text-handler-for-node-type
 ::ast/type.keyword
 (fn [self ctx text seg node ast cursor-pos]
   (let [cursor-idx (:local-idx cursor-pos)]
     (when-not (nil? cursor-idx)
       (let [repr (ast.string/simple-node->string node)
             new-repr (str-insert-at repr cursor-idx text)
             r (keyword-handle-changed-text
                (::clj-editor/state ctx) seg node cursor-pos repr new-repr
                (+ cursor-idx (count text)))]
         (some-> r vector))))))

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
