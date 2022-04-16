(ns chic.clj-editor.parser
  (:require
   [chic.clj-editor.parser.shared :as pshared
    :refer [append-new-node! read-char safe-unread]]
   [chic.clj-editor.parser.macros :as rmacros]
   [better-cond.core :refer [cond] :rename {cond cond+}]
   [proteus :refer [let-mutable]]
   [farolero.core :as far]
   [tech.v3.datatype.char-input :as dtype.char-input]
   [chic.clj-editor.ast :as ast])
  (:import
   (clojure.lang Numbers BigInt)
   (tech.v3.datatype CharReader)
   (java.io Reader)))

(defn read-whitespace* [^CharReader rdr initch]
  (let [sb (doto (StringBuilder.) (.append initch))]
    (loop []
      (let [cn (.read rdr)]
        (case cn
          (9 32)
          (do (.append sb (char cn))
              (recur))
          (do (safe-unread rdr cn)
              (.toString sb)))))))

(defn read-commas* [^CharReader rdr initch]
  (let [sb (doto (StringBuilder.) (.append initch))]
    (loop []
      (let [cn (.read rdr)]
        (case cn
          44
          (do (.append sb (char cn))
              (recur))
          (do (safe-unread rdr cn)
              (.toString sb)))))))

(defn append-whitespace! [parser parent ws]
  (append-new-node! parser parent ::ast/type.whitespace
                    {::ast/node.string ws}))

(defn append-commas! [parser parent s]
  (append-new-node! parser parent ::ast/type.commas
                    {::ast/node.string s}))

(def ^"[Lclojure.lang.IFn;" macros
  (doto ^"[Lclojure.lang.IFn;" (make-array clojure.lang.IFn 256)
    (aset \" rmacros/read-string!)
    (aset \; rmacros/read-comment!)
    (aset \' rmacros/read-quote!)
    (aset \@ rmacros/read-deref!)
    (aset \^ rmacros/read-meta!)
    (aset \` rmacros/read-syntax-quote!)
    (aset \~ rmacros/read-unquote!)
    (aset \( rmacros/read-list!)
    (aset \) rmacros/read-unmatched-delim!)
    (aset \[ rmacros/read-vector!)
    (aset \] rmacros/read-unmatched-delim!)
    (aset \{ rmacros/read-map!)
    (aset \} rmacros/read-unmatched-delim!)
    (aset \\ rmacros/read-char!)
    (aset \% rmacros/read-arg!)
    (aset \# rmacros/read-dispatch!)))

(def ^"[Lclojure.lang.IFn;" disp-macros
  (doto ^"[Lclojure.lang.IFn;" (make-array clojure.lang.IFn 256)
    (aset \^ rmacros/read-meta!)
    (aset \# rmacros/read-symbolic-val!)
    (aset \' rmacros/read-var!)
    (aset \" rmacros/read-regex!)
    (aset \( rmacros/read-fn!)
    (aset \{ rmacros/read-set!)
    (aset \= rmacros/read-eval!)
    (aset \! rmacros/read-comment!)
    (aset \< rmacros/read-unreadable!)
    (aset \_ rmacros/read-discard!)
    (aset \? rmacros/read-conditional!)
    (aset \: rmacros/read-namespaced-map!)))

(defn get-macro [cn]
  (when (< cn (alength macros))
    (aget macros cn)))

(defn match-int [s]
  (let [matcher (re-matcher #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?" s)]
    (when (.matches matcher)
      (if (nil? (.group matcher 2))
        (let-mutable
         [negate? (.equals "-" (.group matcher 1))
          n nil
          radix (cond+
                 :do (set! n (.group matcher 3))
                 (not (nil? n))
                 10
                 :do (set! n (.group matcher 4))
                 (not (nil? n))
                 16
                 :do (set! n (.group matcher 5))
                 (not (nil? n))
                 8
                 :do (set! n (.group matcher 7))
                 (not (nil? n))
                 (Integer/parseInt (.group matcher 6))
                 (far/error ::invalid-radix))
          bn (BigInteger. ^String n ^long radix)]
          (when negate? (set! bn (.negate ^BigInteger bn)))
          (if (nil? (.group matcher 8))
            (if (< (.bitLength ^BigInteger bn) 64)
              (Numbers/num (.longValue ^BigInteger bn))
              (BigInt/fromBigInteger ^BigInteger bn))
            (BigInt/fromBigInteger ^BigInteger bn)))
        (if (nil? (.group matcher 8))
          (Numbers/num 0)
          0N)))))

(defn match-float [s]
  (let [matcher (re-matcher #"([-+]?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?" s)]
    (when (.matches matcher)
      (if (nil? (.group matcher 4))
        (Double/parseDouble s)
        (BigDecimal. (.group matcher 1))))))

(defn match-ratio [s]
  (let [matcher (re-matcher #"([-+]?[0-9]+)/([0-9]+)" s)]
    (when (.matches matcher)
      (let [numerator ^String (.group matcher 1)
            numerator (if (identical? (.charAt numerator 0) \+)
                        (.substring numerator 1)
                        numerator)]
        (Numbers/divide
         (Numbers/reduceBigInt
          (BigInt/fromBigInteger (BigInteger. numerator)))
         (Numbers/reduceBigInt
          (BigInt/fromBigInteger (BigInteger. (.group matcher 2)))))))))

(defn match-number [s]
  (or (match-int s)
      (match-float s)
      (match-ratio s)))

(defn macro?* [cn]
  (and (< cn (alength macros)) (not (nil? (aget macros cn)))))

(defn read-number* [parser ^CharReader rdr initch]
  (let [sb (doto (StringBuilder.) (.append initch))]
    (loop []
      (let [cn (.read rdr)]
        (when-not (== -1 cn)
          (if (or (pshared/macro? parser cn) (pshared/whitespace? cn))
            (.unread rdr)
            (do (.append sb (char cn))
                (recur))))))
    (.toString sb)))

(defn append-number! [parser parent num]
  (append-new-node! parser parent ::ast/type.number
                    (ast/number-node num)))

(defn append-autoresolving-kwd!
  ([parser parent nam]
   (append-new-node! parser parent ::ast/type.keyword
                     (ast/keyword-auto-node nam)))
  ([parser parent alias nam]
   (append-new-node! parser parent ::ast/type.keyword
                     (ast/keyword-alias-node alias nam))))

(defn append-keyword! [parser parent namsp nam]
  (append-new-node! parser parent ::ast/type.keyword
                    (ast/keyword-node namsp nam)))

(defn append-symbol! [parser parent namsp nam]
  (append-new-node! parser parent ::ast/type.symbol
                    (ast/symbol-node namsp nam)))

(def symbol-re-pattern #"([\D&&[^/]].*/)?(/|[\D&&[^/]][^/]*)")

(defn append-symbolic! [parser parent ^String s]
  (let [matcher (re-matcher #"[:]?([\D&&[^/]].*/)?(/|[\D&&[^/]][^/]*)" s)]
    (when (.matches matcher)
      (let [namsp (.group matcher 1)
            nam (.group matcher 2)]
        (when-not (or (and (some? namsp) (.endsWith namsp ":/"))
                      (.endsWith nam ":")
                      (< 0 (.indexOf s "::" 1)))
          (if (.startsWith s "::")
            (if namsp
              (append-autoresolving-kwd! parser parent (.substring namsp 1) nam)
              (append-autoresolving-kwd! parser parent nam))
            (if (identical? \: (.charAt s 0))
              (append-keyword! parser parent namsp nam)
              (append-symbol! parser parent namsp nam))))))))

(defn append-token! [parser parent token]
  (case token
    "nil"
    (pshared/append-child! parser parent ::ast/constant.nil)
    "true"
    (pshared/append-child! parser parent ::ast/constant.true)
    "false"
    (pshared/append-child! parser parent ::ast/constant.false)
    (or (append-symbolic! parser parent token)
        (far/error ::invalid-token {:string token}))))

(defn append-newline! [parser parent ^CharReader rdr initch]
  (when (identical? initch \return)
    (.read rdr))
  ;; (append-new-node! parser parent ::ast/type.newline {})
  (pshared/new-line! parser))

(defn read* [parser parent ^CharReader rdr]
  (let [c (read-char rdr)]
    (case c
      (\tab \space)
      (append-whitespace! parser parent (read-whitespace* rdr c))
      (\newline \return)
      (append-newline! parser parent rdr c)
      \,
      (append-commas! parser parent (read-commas* rdr c))
      (cond+
       (Character/isDigit c)
       (append-number! parser parent (read-number* parser rdr c))

       :let [macrofn (get-macro (int c))]
       (some? macrofn)
       (macrofn parser parent rdr c)

       :let [num? (case c (\- \+)
                        (let [c2 (read-char rdr)]
                          (.unread rdr)
                          (Character/isDigit c2))
                        nil)]
       num?
       (append-number! parser parent (read-number* parser rdr c))

       (append-token! parser parent (pshared/read-token* parser rdr c))))))

(defn read-into [db parent-id line-id start-idx ^Reader rdr]
  (let [rdr (dtype.char-input/reader->char-reader rdr)
        line (get (::ast/lines db) line-id)
        children-after (-> (::ast/nodes db) (get parent-id) (get ::ast/node-children)
                           (subvec start-idx))
        line-order (::ast/line-order db)
        line-idx (util/index-of line-order line-id)
        line-order-after (-> line-order (subvec line-idx))
        tdb (ast/transient-db db parent-id)
        parser (pshared/new-parser tdb #'read* macros disp-macros)]
    (while (not (or (.eof rdr) (== -1 (.read rdr))))
      (.unread rdr)
      (read* parser parent-id rdr))
    (pshared/persist-node! parser parent)
    (pshared/persistent-result! parser)))

(defn read-fresh [^Reader rdr]
  (let [rdr (dtype.char-input/reader->char-reader rdr)
        tdb (ast/transient-db (ast/new-db) ::ast/root-node)
        parser (pshared/new-parser tdb #'read* macros disp-macros)]
    (while (not (or (.eof rdr) (== -1 (.read rdr))))
      (.unread rdr)
      (read* parser ::ast/root-node rdr))
    (pshared/persist-node! parser ::ast/root-node)
    (pshared/persistent-result! parser)))

(defn terminating-macro?* [cn]
  (case (char cn) (\# \' \%) false (macro?* cn)))

(defn read-token* [^CharReader rdr initch]
  (let [sb (doto (StringBuilder.) (.append initch))]
    (loop []
      (let [cn (.read rdr)]
        (when-not (== -1 cn)
          (if (or (pshared/whitespace? cn) (terminating-macro?* cn))
            (.unread rdr)
            (do (.append sb (char cn))
                (recur))))))
    (.toString sb)))

(defn parse-symbol [symstr]
  #_(let [[_ namsp nam]
        (re-matches #"(?:([[\D!-~]&&[^/]].*)/)?(/|[[\D!-~]&&[^/]][[!-~]&&[^/]]*)"
                    symstr)]
      (when nam (symbol namsp nam)))
  (let [rdr (dtype.char-input/reader->char-reader
             (java.io.StringReader. symstr))]
    (when-not (.eof rdr)
      (let [initcn (.read rdr)
            initch (char initcn)]
        (when-not (or (pshared/whitespace? initcn) (case initch \% false (macro?* initcn)))
          (let [token (read-token* rdr initch)]
            (when (.eof rdr)
              (when-some [[_ ^String nspart ^String nampart]
                          (re-matches #"(?:([[!-~\u0080-\uFFFF]&&[^/\d]][[!-~\u0080-\uFFFF]&&[^/]]*)/)?(/|[[!-~\u0080-\uFFFF]&&[^/\d'#]][[!-~\u0080-\uFFFF]&&[^/]]*)"
                                      token)]
                (when-not (or (and (some? nspart)
                                   (or (.endsWith nspart ":")
                                       (identical? \: (.charAt nspart 0))))
                              (.endsWith nampart ":")
                              (identical? \: (.charAt nampart 0))
                              (re-matches #"nil|true|false|(?:\+|-)\d.*" nampart))
                  (symbol nspart
                          nampart))))))))))

(comment
  (= "throws" (parse-symbol nil))
  (name 'a/#
        )
  (name 'v//a )
  (name (read-string "a//0"))
  (name (symbol "a///b"))

  (re-matches #"[[!-~\u0080-\uFFFF]&&[^/\d]].*" "a//")
  (and
   (and
    (= 'hello/there (parse-symbol "hello/there"))
    (= 'there (parse-symbol "there")))
   (and
    (nil? (parse-symbol "+5"))
    (nil? (parse-symbol "ns/+5"))
    (nil? (parse-symbol "-5"))
    (nil? (parse-symbol "ns/-5"))
    (nil? (parse-symbol "true"))
    (nil? (parse-symbol "ns/true"))
    (nil? (parse-symbol "false"))
    (nil? (parse-symbol "nil"))
    (nil? (parse-symbol "ns/nil")))
   (and
    (= '/ (parse-symbol "/"))
    (= 'ns// (parse-symbol "ns//"))
    ;; clojure supports ns//x but not /x
    ;; similar for ns/# and ns/'
    (nil? (parse-symbol "ns/#"))
    (nil? (parse-symbol "ns/#a"))
    (nil? (parse-symbol "ns/'a"))
    (nil? (parse-symbol "ns/'"))
    (nil? (parse-symbol "ns///0"))
    (nil? (parse-symbol "ns///"))
    (nil? (parse-symbol "ns////x"))
    (nil? (parse-symbol "a/"))
    (nil? (parse-symbol "/a")))
   (and
    (= 'a## (parse-symbol "a##"))
    (= 'a#b (parse-symbol "a#b"))
    (= '.a.b (parse-symbol ".a.b"))
    (= '%b (parse-symbol "%b"))
    (= 'a%b (parse-symbol "a%b")))
   (and
    (nil? (parse-symbol ""))
    (nil? (parse-symbol " "))
    (nil? (parse-symbol "a b"))
    (nil? (parse-symbol ","))
    (nil? (parse-symbol "\t"))
    (nil? (parse-symbol " ab \t"))
    (nil? (parse-symbol "ab\t")))
   (and
    (= 'a0 (parse-symbol "a0"))
    (= 'a/a0 (parse-symbol "a/a0"))
    (= 'a0/a (parse-symbol "a0/a"))
    (nil? (parse-symbol "0"))
    (nil? (parse-symbol "0a"))
    (nil? (parse-symbol "a/0a"))
    (nil? (parse-symbol "0a/a")))
   (and
    (nil? (parse-symbol "\b"))
    (nil? (parse-symbol "a\b"))
    (nil? (parse-symbol "a\b/b"))
    (nil? (parse-symbol "a/a\b")))
   (and
    (nil? (parse-symbol "()"))
    (nil? (parse-symbol "#b"))
    (nil? (parse-symbol "\""))
    (nil? (parse-symbol "@"))
    (nil? (parse-symbol "\\")))
   (and
    (= 'a:b:c (parse-symbol "a:b:c"))
    (= 'a:b/a:c (parse-symbol "a:b/a:c"))
    (nil? (parse-symbol ":a"))
    (nil? (parse-symbol "a:"))
    (nil? (parse-symbol "a:/a"))
    (nil? (parse-symbol "a/:a")))
   (= (symbol "\u00D7") (parse-symbol "\u00D7")))


  #!
  )

(defn constant? [s]
  (some? (re-matches #"true|false|nil" s)))

#_(defn parse-keyword->node [s]
  )
