(ns chic.clj-editor.parser.shared
  (:require
   [chic.clj-editor.ast :as ast]
   [farolero.core :as far])
  (:import
   (tech.v3.datatype CharReader)))

(defprotocol PParser
  (persistent-result! [_])
  (get-node [_ id])
  (macro? [_ cn])
  (append-child! [_ parent-id id] "Used for constants")
  (new-line! [_])
  (get-dispatch-macro [_ c])
  (read-node [_ parent-id rdr])
  (persist-node! [_ id])
  (append-new-node! [_ parent-id typ node])
  (append-transient-node-with-children! [_ parent-id typ node]))

(deftype Parser [tdb
                 ^:unsynchronized-mutable tnodes
                 ^:unsynchronized-mutable tlines
                 ^:unsynchronized-mutable tline
                 ^:unsynchronized-mutable tline-order
                 node-reader
                 ^"[Lclojure.lang.IFn;" dispatch-macros
                 ^"[Lclojure.lang.IFn;" macros
                 ^:unsynchronized-mutable ^long id-counter]
  PParser
  (persistent-result! [self]
    (set! tline (pop! tline)) ;; exclude closing ast/root-node
    (new-line! self)
    (-> tdb
        (assoc! ::ast/nodes (persistent! tnodes))
        (assoc! ::ast/lines (persistent! tlines))
        (assoc! ::ast/line-order (persistent! tline-order))
        (assoc! :id-counter (volatile! (unchecked-inc id-counter)))
        persistent!))
  (persist-node! [_ id]
    (let [node (get tnodes id)
          node (assoc! node ::ast/node-children
                       (persistent! (get node ::ast/node-children)))]
      (set! tnodes (assoc! tnodes id (persistent! node)))
      (set! tline (conj! tline [::ast/seg.comp-end id]))
      nil))
  (new-line! [_]
    (set! tlines (assoc! tlines
                         (set! id-counter (unchecked-inc id-counter))
                         (persistent! tline)))
    (set! tline (transient []))
    (set! tline-order (conj! tline-order id-counter))
    nil)
  (append-child! [_ parent-id id]
    (let [parent (get tnodes parent-id)
          children (conj! (get parent ::ast/node-children) id)
          parent (assoc! parent ::ast/node-children children)]
      (set! tnodes (assoc! tnodes parent-id parent))
      (set! tline (conj! tline [::ast/seg.simple id]))
      nil))
  (append-new-node! [_ parent-id typ node]
    (let [parent (get tnodes parent-id)
          id (set! id-counter (unchecked-inc id-counter))
          children (conj! (get parent ::ast/node-children) id)
          parent (assoc! parent ::ast/node-children children)
          ;; nodes (assoc! nodes parent-id parent)
          node (assoc node ::ast/node-type typ)]
      (set! tnodes (assoc! tnodes id node))
      (set! tline (conj! tline [::ast/seg.simple id]))
      id))
  (append-transient-node-with-children! [_ parent-id typ node]
    (let [id (set! id-counter (unchecked-inc id-counter))
          parent (get tnodes parent-id)
          children (conj! (get parent ::ast/node-children) id)
          parent (assoc! parent ::ast/node-children children)
          ;; nodes (assoc! nodes parent-id parent)
          node (-> (transient node)
                   (assoc! ::ast/node-type typ)
                   (assoc! ::ast/node-children (transient [])))]
      (set! tnodes (assoc! tnodes id node))
      (set! tline (conj! tline [::ast/seg.comp-start id]))
      id))
  (get-node [_ id]
    (get tnodes id))
  (read-node [self parent-id rdr]
    (node-reader self parent-id rdr))
  (get-dispatch-macro [_ c]
    (aget dispatch-macros c))
  (macro? [_ cn]
    (and (< cn (alength macros)) (not (nil? (aget macros cn))))))

(defn new-parser [tdb node-reader macros disp-macros]
  (Parser. tdb (::ast/nodes tdb) (::ast/lines tdb)
           (transient []) (transient []) node-reader disp-macros macros @(:id-counter tdb)))

(defn ^Character read-char [^CharReader rdr]
  (let [cn (.read rdr)]
    (if (== -1 cn)
     (far/error ::end-of-input)
     (char cn))))

(defn safe-unread [^CharReader rdr cn]
  (when-not (== -1 cn)
    (.unread rdr)))

(defn terminating-macro? [parser cn]
  (case (char cn) (\# \' \%) false (macro? parser cn)))

(defn whitespace? [^long cn]
  (or (Character/isWhitespace cn) (clojure.lang.Util/identical cn 44)))

(defn read-token* [parser ^CharReader rdr initch]
  (let [sb (doto (StringBuilder.) (.append initch))]
    (loop []
      (let [cn (.read rdr)]
        (when-not (== -1 cn)
          (if (or (whitespace? cn) (terminating-macro? parser cn))
            (.unread rdr)
            (do (.append sb (char cn))
                (recur))))))
    (.toString sb)))
