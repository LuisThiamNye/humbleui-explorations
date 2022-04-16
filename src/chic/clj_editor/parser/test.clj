(ns chic.clj-editor.parser.test
  (:require
   [tech.droit.fset :as fset]
   [malli.core :as m]
   [chic.clj-editor.lines :as lines]
   [criterium.core :as crit]
   [io.github.humbleui.profile :as profile]
   [chic.clj-editor.ast :as ast]
   [chic.clj-editor.parser :as parser]))

(def node-id-schema [:or keyword? integer?])

(def comp-node-types-with-end
  #{::ast/type.vector
    ::ast/type.list
    ::ast/type.set
    ::ast/type.map
    ::ast/type.fn})

(def comp-node-wrapper-types
  "Prefix + Whitespace* + child"
  #{::ast/type.quote
    ::ast/type.syntax-quote
    ::ast/type.unquote
    ::ast/type.unquote-splicing
    ::ast/type.conditional
    ::ast/type.splicing-conditional
    ::ast/type.deref
    ::ast/type.discard
    ::ast/type.eval

    ;; complex prefixes
    ::ast/type.ns-map
    ::ast/type.tagged})

(def comp-node-types
  (fset/union
   #{::ast/type.meta
     }
   comp-node-types-with-end
   comp-node-wrapper-types))

(def simple-node-types
  #{::ast/type.constant
    ::ast/type.symbol
    ::ast/type.arg
    ::ast/type.keyword
    ::ast/type.var

    ::ast/type.number

    ::ast/type.symbolic-val
    ::ast/type.char

    ;; multiline
    ::ast/type.regex
    ::ast/type.string

    ;; whitespace
    ::ast/type.whitespace
    ::ast/type.commas
    ::ast/type.comment ;; to end of line
    })

(def segment-schema
  [:tuple
   [:enum
    ::ast/seg.comp-end
    ::ast/seg.comp-start
    ::ast/seg.simple]
   node-id-schema])

(def tdb-schema
  [:map
   [::ast/nodes
    [:map-of
     node-id-schema
     [:map
      [::ast/node-type keyword?]
      [::ast/node-children {:optional true}
       [:vector node-id-schema]]]]]
   [::ast/lines
    [:map-of integer?
     [:vector
      segment-schema]]]
   [::ast/line-order
    [:vector integer?]]])

(defn- read-str [s]
  (parser/read-fresh (java.io.StringReader. s)))

(defn test-strs [strs]
  (loop [results []
         strs strs]
    (if-let [s (first strs)]
      (let [r (try (read-str s)
                   (catch Throwable e
                     e))]
        (recur
         (conj results
               [s r (when-not (instance? Throwable r)
                      (m/explain @#'tdb-schema r))])
         (next strs)))
      results)))

(def sample-strs
  [""
   "\t "
   "sym"
   "ns/sym"
   ":kwd"
   ":ns/kwd"
   "::autokwd"
   "::ns/autokwd"
   "42"
   "42.01"
   "0xa8"
   "+3"
   "-3"
   "34/2"
   "##Inf"
   "^:x sym"
   "#_ #_ sym sym"
   "[1 2 3]"
   "{1 2 3}"
   "#:ns {:a map}"
   " \"a string\" "
   " \"a \\\"string\" "
   " #\"a regex\" "
   "(a b c)"
   "()"
   "(a(b()))"
   "\\] "
   "((%1))"])

(def sample-file-str (slurp "src/chic/graph.clj"))
(def sample-small (slurp "src/chic/util.clj"))

(comment
  (tap> (read-str (slurp "src/chic/graph.clj")))
  (tap> (read-str (slurp "resources/clj/test_clj_syntax.clj")))
  (tap> {:a 4})
  (tap> 4)
  (let [s (str (read-str (slurp "src/chic/graph.clj")))]
    ;; (time (do nil))
    (time (subs s 0 100)))
  (tap>
   (filterv (fn [[_ result m]]
              (or m (instance? Throwable result)))
            (test-strs sample-strs)))
  (count (::ast/nodes (read-str (slurp "src/chic/graph.clj"))))
  (do
    (profile/reset)
    ;; (profile/measure "total" (read-str sample-file-str))
    (profile/measure "total" (read-str sample-small))
    (profile/log))
  (crit/quick-bench (type (read-str sample-file-str)))
  (crit/quick-bench (type (read-str sample-small)))
  (type (::ast/nodes (read-str "")))

  (count (str (read-str (slurp "src/chic/graph.clj"))))
  (count (slurp "src/chic/graph.clj"))
  (def --nodes (::ast/nodes (read-str (slurp "src/chic/graph.clj"))))
  (do (time (read-string (slurp "src/chic/graph.clj"))) nil)
  ;; (time (do (rewrite-clj.parser/parse-string (slurp "src/chic/graph.clj")) nil))

  (lines/nodes->lines --nodes)
  (lines/nodes->lines (::ast/nodes (read-str "\n\n")))

  #!
  )
