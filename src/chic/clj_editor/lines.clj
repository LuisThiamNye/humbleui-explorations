(ns chic.clj-editor.lines
  (:require
   [chic.util :as util]
   [clj-commons.primitive-math :as prim]
   [chic.bifurcan :as b]
   [chic.clj-editor.ast :as ast]))

(declare nodes->lines*)

(defn nodes->lines*-children [nodes lines line-mut children]
  (reduce (fn [lines child-id]
           (nodes->lines* nodes child-id lines line-mut))
         lines
         children))

(defn nodes->lines* [nodes id lines line-mut]
  (let [node (get nodes id)]
    (if (ast/newline? node)
      (let [r (conj! lines (persistent! @line-mut))]
        (util/mreset! line-mut (transient []))
        r)
      (if-let [children (get node ::ast/node-children)]
        (do
          (util/mreset! line-mut
                        (conj! @line-mut [::start-of id]))
          (let [lines (nodes->lines*-children nodes lines line-mut children)]
            (util/mreset! line-mut
                          (conj! @line-mut [::end-of id]))
            lines))
        (do (util/mreset!
             line-mut (conj! @line-mut id))
            lines)))))

(defn nodes->lines [nodes]
  (let [line-mut (util/mutable! (transient []))]
    (persistent!
     (conj! (nodes->lines*-children
             nodes (transient []) line-mut
             (::ast/node-children (get nodes ::ast/root-node)))
            (persistent! @line-mut)))))
