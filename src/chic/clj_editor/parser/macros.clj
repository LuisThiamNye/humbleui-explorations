(ns chic.clj-editor.parser.macros
  (:require
   [chic.bifurcan :as b]
   [chic.util :as util]
   [chic.clj-editor.parser.shared :as pshared
    :refer [append-new-node! read-char]]
   [chic.clj-editor.ast :as ast]
   [farolero.core :as far])
  (:import
   (tech.v3.datatype CharReader)))

(defn read-string! [parser parent ^CharReader rdr _doublequote]
  (let [sb (StringBuilder.)]
    (loop []
      (let [c (read-char rdr)]
        (when-not (identical? \" c)
          (.append sb c)
          (when (identical? \\ c)
            (let [c2 (read-char rdr)]
              (.append sb c2)))
          (recur))))
    (append-new-node! parser parent ::ast/type.string
                      {::ast/string.string (.toString sb)})))

(defn read-comment! [parser parent ^CharReader rdr _semicolon]
  (let [sb (StringBuilder.)]
    (loop []
      (let [cn (.read rdr)]
        (when-not (== -1 cn)
          (case cn
            (10 13)
            (.unread rdr)
            (do (.append sb (char cn))
                (recur))))))
    (append-new-node! parser parent ::ast/type.comment
                      {::ast/comment.contents (.toString sb)
                       ::ast/comment.prefix ";"})))

(defn read-children-until-nonws! [parser parent ^CharReader rdr]
  (loop []
    (let [child-id (pshared/read-node parser parent rdr)
          child (pshared/get-node parser child-id)]
      (when (ast/whitespace? child)
        (recur)))))

(defn append-wrapper!
  ([parser parent ^CharReader rdr typ]
   (append-wrapper! parser parent rdr typ (b/hmap {})))
  ([parser parent ^CharReader rdr typ m]
   (let [wrapper-id (pshared/append-transient-node-with-children!
                     parser parent typ m)]
     (read-children-until-nonws! parser wrapper-id rdr)
     (pshared/persist-node! parser wrapper-id)
     wrapper-id)))

(defn read-quote! [parser parent ^CharReader rdr _singlequote]
  (append-wrapper! parser parent rdr ::ast/type.quote))

(defn read-deref! [parser parent ^CharReader rdr _at-char]
  (append-wrapper! parser parent rdr ::ast/type.deref))

(defn read-meta! [parser parent ^CharReader rdr _caret]
  (let [wrapper-id (pshared/append-transient-node-with-children!
                    parser parent ::ast/type.meta (b/hmap {}))]
    (read-children-until-nonws! parser wrapper-id rdr)
    (read-children-until-nonws! parser wrapper-id rdr)
    (pshared/persist-node! parser wrapper-id)
    wrapper-id))

(defn read-syntax-quote! [parser parent ^CharReader rdr _backquote]
  (append-wrapper! parser parent rdr ::ast/type.syntax-quote))

(defn read-unquote! [parser parent ^CharReader rdr _tilde]
  (let [c (read-char rdr)]
    (if (identical? c \@)
      (append-wrapper! parser parent rdr ::ast/type.unquote-splicing)
      (do (.unread rdr)
          (append-wrapper! parser parent rdr ::ast/type.unquote)))))

(defn append-delimited-list! [parser parent ^CharReader rdr delim typ]
  (let [list-id (pshared/append-transient-node-with-children!
                 parser parent typ (b/hmap {}))]
    (loop []
     (when-not (identical? delim (read-char rdr))
       (.unread rdr)
       (do (pshared/read-node parser list-id rdr)
           (recur))))
    (pshared/persist-node! parser list-id)
    list-id))

(defn read-list! [parser parent ^CharReader rdr _openparen]
  (append-delimited-list! parser parent rdr (util/compile (first ")"))
                          ::ast/type.list))

(defn read-vector! [parsre parent ^CharReader rdr _openbracket]
  (append-delimited-list! parsre parent rdr (util/compile (first "]"))
                          ::ast/type.vector))

(defn read-map! [parser parent ^CharReader rdr _openbracket]
  (append-delimited-list! parser parent rdr (util/compile (first "}"))
                          ::ast/type.map))

(defn read-unmatched-delim! [_parser _parent ^CharReader rdr delim]
  (far/error ::pshared/unmatched-delimiter
             {:delim delim
              :pos (.position rdr)}))

(defn read-char! [parser parent ^CharReader rdr _backslash]
  (let [token (pshared/read-token* parser rdr (read-char rdr))]
    (append-new-node! parser parent ::ast/type.char {::ast/char.token token})))

(defn read-arg! [parser parent ^CharReader rdr _percent]
  (let [c (read-char rdr)
        n (if (or (pshared/whitespace? (int c))
                  (pshared/terminating-macro? parser (int c)))
            (do (.unread rdr) nil)
            c)]
    (append-new-node! parser parent ::ast/type.arg (if n {::ast/arg.n n} {}))))

(defn read-ctor! [parser parent ^CharReader rdr initch]
  (let [tag (pshared/read-token* parser rdr initch)
        [_ tag-ns tag-name] (re-matches #"(?:(.*/))?(.+)" tag)
        ctor-id (pshared/append-transient-node-with-children!
                 parser parent ::ast/type.tagged
                 (b/hmap {::ast/tagged.tag-ns tag-ns
                          ::ast/tagged.tag-name tag-name}))]
    (read-children-until-nonws! parser ctor-id rdr)
    (pshared/persist-node! parser ctor-id)
    ctor-id))

(defn read-dispatch! [parser parent ^CharReader rdr _hash]
  (let [c (read-char rdr)]
    (if-some [f (pshared/get-dispatch-macro parser c)]
      (f parser parent rdr c)
      (read-ctor! parser parent rdr c))))

(defn read-symbolic-val! [parser parent ^CharReader rdr _hash]
  (let [token (pshared/read-token* parser rdr (read-char rdr))]
    (append-new-node! parser parent ::ast/type.symbolic-val
                      {::ast/symbolic-val.token token})))

(defn read-var! [parser parent ^CharReader rdr _quote]
  (let [token (pshared/read-token* parser rdr (read-char rdr))
        [_ namsp nam] (re-matches #"(?:(.*/))?(.+)" token)]
    (append-new-node! parser parent ::ast/type.var
                      {::ast/symbol.ns namsp
                       ::ast/symbol.name nam})))

(defn read-regex! [parser parent ^CharReader rdr _doublequote]
  (let [sb (StringBuilder.)]
    (loop []
      (let [c (read-char rdr)]
        (when-not (identical? \" c)
          (.append sb c)
          (when (identical? \\ c)
            (let [c2 (read-char rdr)]
              (.append sb c2)))
          (recur))))
    (append-new-node! parser parent ::ast/type.regex
                      {::ast/string.string (.toString sb)})))

(defn read-fn! [parser parent ^CharReader rdr _openparen]
  (append-delimited-list! parser parent rdr (util/compile (first ")"))
                          ::ast/type.fn))

(defn read-set! [parser parent ^CharReader rdr _openbrace]
  (append-delimited-list! parser parent rdr (util/compile (first "}"))
                          ::ast/type.set))

(defn read-eval! [parser parent ^CharReader rdr _equals]
  (append-wrapper! parser parent rdr ::ast/type.eval))

(defn read-unreadable! [_parser _parent ^CharReader _rdr _openchevron]
  (far/error ::pshared/unreadable-form))

(defn read-discard! [parser parent ^CharReader rdr _]
  (append-wrapper! parser parent rdr ::ast/type.discard))

(defn read-conditional! [parser parent ^CharReader rdr _qmark]
  (let [c (read-char rdr)]
    (if (identical? c \@)
      (append-wrapper! parser parent rdr ::ast/type.conditional)
      (do (.unread rdr)
          (append-wrapper! parser parent rdr ::ast/type.splicing-conditional)))))

(defn read-namespaced-map! [parser parent ^CharReader rdr _colon]
  (let [auto? (or (identical? (read-char rdr) \:)
                  (do (.unread rdr) false))
        namsp (pshared/read-token* parser rdr (read-char rdr))]
    (append-wrapper! parser parent rdr ::ast/type.ns-map
                     (b/hmap {::ast/ns-map.auto? auto?
                              ::ast/ns-map.ns namsp}))))
