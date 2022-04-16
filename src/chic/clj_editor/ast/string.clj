(ns chic.clj-editor.ast.string
  (:require
   [chic.clj-editor.ast :as ast]))

(def simple-node-type->strfn
  {::ast/type.whitespace
   (fn [node]
     (::ast/node.string node))
   ::ast/type.comment
   (fn [node]
     (str (::ast/comment.prefix node)
          (::ast/comment.contents node)))
   ::ast/type.keyword
   (fn [node]
     (str (if (::ast/keyword.auto? node)
            "::"
            ":")
          (when-let [namsp (::ast/symbol.ns node)]
            (str namsp #_"/"))
          (::ast/symbol.name node)))
   ::ast/type.symbol
   (fn [node]
     (str (::ast/symbol.ns node)
          (::ast/symbol.name node)))
   ::ast/type.string
   (fn [node]
     (str \" (::ast/string.string node) \"))
   ::ast/type.regex
   (fn [node]
     (str "#\"" (::ast/string.string node) "\""))
   ::ast/type.constant
   (fn [node]
     (pr-str (::ast/constant.value node)))
   ::ast/type.number
   (fn [node]
     (::ast/number.string node))
   ::ast/type.arg
   (fn [node]
     (str "%" (::ast/arg.n node)))
   ::ast/type.var
   (fn [node]
     (str "#'"
          (::ast/symbol.ns node)
          (::ast/symbol.name node)))
   ::ast/type.symbolic-val
   (fn [node]
     (str "##" (::ast/symbolic-val.token node)))
   ::ast/type.char
   (fn [node]
     (str "\\" (::ast/char.token node)))})

(defn simple-node->string [node]
  (if-let [f (simple-node-type->strfn (::ast/node-type node))]
    (f node)
    (do (println "not found stringfn for" (::ast/node-type node)) "")))

(def ^java.util.HashMap composite-node-type->strfn-start
  (java.util.HashMap.))

(defn composite-node->string-start [node]
  (if-let [f (.getOrDefault composite-node-type->strfn-start
                            (::ast/node-type node) nil)]
    (f node)
    (do (println "not found start stringfn for" (::ast/node-type node)) "")))

(def ^java.util.HashMap composite-node-type->strfn-end
  (java.util.HashMap.))

(defn composite-node->string-end [node]
  (if-let [f (.getOrDefault composite-node-type->strfn-end
                            (::ast/node-type node) nil)]
    (f node)
    (do (println "not found end stringfn for" (::ast/node-type node)) "")))

(defmacro add-comp-node-strfns [typ startf endf]
  (.put composite-node-type->strfn-start typ (eval startf))
  (.put composite-node-type->strfn-end typ (eval endf))
  nil)

(def constantly-empty-str (fn [_] ""))

(add-comp-node-strfns
 ::ast/type.map
 (fn [node] "{")
 (fn [node] "}"))

(add-comp-node-strfns
 ::ast/type.vector
 (fn [node] "[")
 (fn [node] "]"))

(add-comp-node-strfns
 ::ast/type.set
 (fn [node] "#{")
 (fn [node] "}"))

(add-comp-node-strfns
 ::ast/type.list
 (fn [node] "(")
 (fn [node] ")"))

(add-comp-node-strfns
 ::ast/type.deref
 (fn [node] "@")
 constantly-empty-str)

(add-comp-node-strfns
 ::ast/type.quote
 (fn [node] "'")
 constantly-empty-str)

(add-comp-node-strfns
 ::ast/type.syntax-quote
 (fn [node] "`")
 constantly-empty-str)

(add-comp-node-strfns
 ::ast/type.unquote
 (fn [node] "~")
 constantly-empty-str)

(add-comp-node-strfns
 ::ast/type.unquote-splicing
 (fn [node] "~@")
 constantly-empty-str)

(add-comp-node-strfns
 ::ast/type.discard
 (fn [node] "#_")
 constantly-empty-str)

(add-comp-node-strfns
 ::ast/type.meta
 (fn [node] "^")
 constantly-empty-str)

(add-comp-node-strfns
 ::ast/type.fn
 (fn [node] "#(")
 (fn [node] ")"))
