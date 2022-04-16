(ns chic.clj-editor.ast
  (:require
   [chic.bifurcan :as b]))

(defn constant-node [value]
  {::node-type ::type.constant
   ::constant.value value})

(defn new-db []
  (b/with-colls [:map]
    {::nodes {::root-node {::node-type ::type.root
                           ::node-children []}
              ::constant.nil (constant-node nil)
              ::constant.true (constant-node true)
              ::constant.false (constant-node false)}
     ::lines {}
     :id-counter (volatile! Long/MIN_VALUE)}))

(defn transient-db [db parent-id]
  (let [db (transient db)
        nodes (transient (::nodes db))
        parent (transient (get nodes parent-id))
        children (transient (::node-children parent))
        parent (assoc! parent ::node-children children)
        nodes (assoc! nodes parent-id parent)
        lines (transient (::lines db))
        db (assoc! db ::nodes nodes)
        db (assoc! db ::lines lines)]
    db))

(defn new-id [db]
  (vswap! (:id-counter db) unchecked-inc))

(defn whitespace? [node]
  (#{::type.whitespace ::type.commas} (::node-type node)))

(defn newline? [node]
  (= ::type.newline (::node-type node)))

#_(defn next-line-id-of-id [line-order line-id]
  (util/index-of line-order line-id))

(defn line-seg->node-id [seg]
  (nth seg 1))

(defn symbol-node
  ([sym] (symbol-node (some-> (namespace sym) (str "/")) (name sym)))
  ([namsp nam]
   {::node-type ::type.symbol
    ::symbol.ns namsp
    ::symbol.name nam}))

(defn number-node [numstr]
  {::node-type ::type.number
   ::number.string numstr})

(defn char-node [token]
  {::node-type ::type.char
   ::char.token token})

(defn symbolic-val-node [token]
  {::node-type ::type.symbolic-val
   ::symbolic-val.token token})

(defn var-node
  ([sym] (var-node (some-> (namespace sym) (str "/")) (name sym)))
  ([namsp nam]
   {::node-type ::type.var
    ::symbol.ns namsp
    ::symbol.name nam}))

(defn arg-node [n]
  (cond-> {::node-type ::type.arg}
    n
    (assoc ::arg.n n)))

(defn keyword-auto-node [nam]
  {::node-type ::type.keyword
   ::keyword.auto? true
   ::symbol.name nam})

(defn keyword-alias-node
  ([sym] (keyword-alias-node (some-> (namespace sym) (str "/")) (name sym)))
  ([alias nam]
   {::node-type ::type.keyword
    ::keyword.auto? true
    ::symbol.ns alias
    ::symbol.name nam}))

(defn keyword-node
  ([sym] (keyword-node (some-> (namespace sym) (str "/")) (name sym)))
  ([namsp nam]
   {::node-type ::type.keyword
    ::symbol.ns namsp
    ::symbol.name nam}))
