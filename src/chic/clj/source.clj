(ns chic.clj.source
  (:require
   [clojure.repl :as repl]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure-lsp.db :as lsp.db]
   [clojure-lsp.handlers :as lsp.handlers]
   [clojure-lsp.api :as lsp.api])
  (:import
   (java.io Reader PushbackReader)))

(defn reader-skip-lines [^Reader rdr nlines]
  (when (< 0 nlines)
    (while (let [c (.read rdr)]
             (not (or (== c 10)
                      (== c -1)))))
    (recur rdr (unchecked-dec-int nlines))))

(defn read-var-source-code [v]
  (let [mmap (meta v)]
    (with-open [rdr ^Reader (io/reader (:file mmap))]
      (reader-skip-lines rdr (unchecked-dec-int (:line mmap)))
      (.skip rdr (unchecked-dec-int (:column mmap)))
      (read (PushbackReader. rdr)))))

(defn crude-source-of-var [v]
  (when (= \/ (first (:file (meta v))))
    (alter-meta! v (fn [m]
                     (update m :file
                             (fn [f]
                               (str/replace f #"/Volumes/House/prg/chic/src/" ""))))))
  (try
    (push-thread-bindings {Compiler/LOADER (.getClassLoader Compiler)})
    (repl/source-fn (symbol v))
    (catch Exception _ nil)
    (finally
      (pop-thread-bindings))))

(comment
  (read-var-source-code #'chic.cljbwr/basic-view)
  (meta #'chic.main/app)
  (meta #'chic.cljbwr/basic-view)

  (meta *ns*)

  ;; what is a namespace file?
  ;; - ns form
  ;; - def macros -> vars
  ;; - extend / extend-type
  ;; - comments lines and blocks
  ;; - declare

  ;; clojure.repl/source-fn depends on reading the file from disk
  ;;   so is unreliable if the file has changed in meantime
  ;; also need to make sure :file is relative in the metadata:
  ;;  (caused when defing new var from nrepl)
  (alter-meta! #'chic.cljbwr/editor-view
               (fn [m]
                 (update m :file
                         (fn [f]
                           (str/replace f #"/Volumes/House/prg/chic/src/" "")))))

  (def lspthing (lsp.api/analyze-project-and-deps! {:file (io/file ".")}))
  (chic.debug/println-main (:message (lsp.api/diagnostics {:namespace '[chic.cljbwr]
                              :output {:canonical-paths true}})))
  (chic.debug/println-main (:message (lsp.api/diagnostics {:namespace '[chic.clj.source]
                                                           :output {:canonical-paths true}})))
  (lsp.handlers/did-change
   {:textDocument {:uri
                   :version}
    :contentChanges [{:start {:line start-line
                              :character start-character}
                      :end {:line end-line
                            :character end-character}}]})
  (-> @lsp.db/db :documents keys)

  (let [x(fn [])]
    x)
  (letfn [(x [])]
    x)
  (require '[clj-kondo.core :as kondo])
  (def ana (atom nil))
  (hash(:analysis
        (kondo/run! {:lint [(io/file "src/chic/clj/source.clj")]
                     :cache true
                     :config {:output {:analysis {:arglists true
                                                  :locals true
                                                  :keywords false
                                                  :protocol-impls true
                                                  :java-class-definitions true}
                                       :canonical-paths true}}
                     :custom-lint-fn (fn [{:keys [analysis] :as kondo-ctx}]
                                       (reset! ana analysis))})))
  @ana

  ;; note: can use clj kondo by giving "-" as path name and using *in*
  ;; `cache-dir`/.cache (default cfg-dir/.cache (cfg-dir usually local to project))
  ;; directory is used to store things like arity info of functions
  ;;  using this is controlled by :cache. Even when :cache is true, the file is read afresh
  ;;  and may update the cache
  ;; kondo works by analysing single files and depending on the cache and config for additional context

  *source-path*

  #!
  )

(let [x 7]
  (def x x))
