(ns chic.clj-editor.ast
  (:require
   [chic.bifurcan :as b]))

(defn new-db []
  (b/with-colls [:map]
    {::nodes {::root-node {::node-type ::type.root
                           ::node-children []}
              ::constant.nil {::node-type ::type.constant
                              ::constant.value nil}
              ::constant.true {::node-type ::type.constant
                               ::constant.value true}
              ::constant.false {::node-type ::type.constant
                                ::constant.value false}}
     ::lines {}
     :id-counter (volatile! Long/MIN_VALUE)}))

(defn new-transient-db []
  (let [db (transient (new-db))
        nodes (transient (::nodes db))
        root-node (transient (::root-node nodes))
        children (transient (::node-children root-node))
        root-node (assoc! root-node ::node-children children)
        nodes (assoc! nodes ::root-node root-node)
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
