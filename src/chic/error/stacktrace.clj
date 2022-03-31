(ns chic.error.stacktrace
  (:require
   [chic.clj.source :as clj.source]
   [chic.debug :as debug]
   [chic.style :as style]
   [chic.ui :as cui]
   [clojure.repl :as repl]
   [clojure.string :as str]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Font Paint]))

(defn expandable-item [toggle contents]
  (let [*expanded? (volatile! false)]
    (cui/clickable
     (fn [event]
       (when (:hui.event.mouse-button/is-pressed event)
         (vswap! *expanded? not)))
     (ui/dynamic ctx [expanded? @*expanded?]
       (ui/column
        toggle
        (when expanded?
          contents))))))

(defn classname->sym [clsn]
  (some->  (re-matches #".+/(?:[^/]+|/)" (repl/demunge clsn)) symbol))

(defn source-of-sym [sym]
  (or (when-let [v (resolve sym)]
        (clj.source/crude-source-of-var v))
      "not found"))

(defn source-of-classname [clsname]
  (let [dm (repl/demunge clsname)]
    (if-let [sym (classname->sym clsname)]
      (source-of-sym sym)
      "-")))

(comment
  (source-of-classname "chic.ui$draw_child")
  (repl/source-fn 'io.github.humbleui.ui.VScrollbar/-draw)
  (chic.debug/println-main (repl/source-fn 'repl/source-fn))
  (classname->sym "clojure.core$_SLASH_")


  #!
  )

(defn stack-trace-view [throwable]
  (let [font-ui (Font. style/face-code-default (float 12))
        fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
        file-col-width 110
        text-vpadding 3
        file-label (fn [filename]
                     (ui/width
                      file-col-width
                      (ui/halign
                       1 (ui/row
                          (ui/padding
                           0 text-vpadding (ui/label (if (= "NO_SOURCE_FILE" filename)
                                           "" filename) font-ui fill-text))
                          (ui/gap 5 0)))))
        line-number-seg (fn [n]
                          (ui/clip
                           (ui/width
                            40
                            (ui/halign
                             1 (ui/row
                                (ui/padding
                                 0 text-vpadding (ui/label (str n) font-ui fill-text))
                                (ui/gap 8 0))))))
        ns-foreign-fill (doto (.makeClone fill-text)
                          (.setAlpha 128))
        ns-fill (doto (Paint.) (.setColor (unchecked-int 0xC0003040)))
        ns-label (fn [clsname]
                   (ui/label clsname font-ui (if (str/starts-with? clsname "chic")
                                               ns-fill ns-foreign-fill)))
        namespaced-seg (fn [clsname]
                         (let [[_ nspart namepart] (re-matches #"(.+/)([^/]+|/)"(repl/demunge clsname))]
                           (ui/row
                            (when nspart
                              (ui/padding
                              0 text-vpadding (ns-label nspart)))
                            (ui/padding
                             0 text-vpadding (ui/label (or namepart clsname) font-ui fill-text)))))
        source-view (fn [content idx]
                      (ui/padding
                       7 0(ui/column
                           (for [[i line] (map-indexed #(vector %1 %2) (str/split-lines content))]
                             (ui/fill
                              (doto (Paint.) (.setColor (unchecked-int (if (= i idx)
                                                                         0x50D0a020
                                                                         0x00000000))))
                              (ui/padding
                               0 text-vpadding
                               (ui/label line font-ui fill-text))))
                        (ui/gap 0 10))))]
    (ui/column
     (loop [children []
            frames (seq (.getStackTrace throwable))]
       #_[(.getFileName frame)
          (.getClassName frame)
          (.getMethodName frame)
          (.getLineNumber frame)
          (.isNativeMethod frame)]
       (if-let [frame (first frames)]
         (let [clsname (.getClassName frame)]
           (cond
             (some #(= % "clojure.lang.Fn") (map #(.getName %) (ancestors (Class/forName clsname))))
             (cond
               (and (= "invokeStatic" (.getMethodName frame))
                    (= "invoke" (.getMethodName (second frames))))
               (let [primaryframe (second frames)
                     sym (classname->sym clsname)
                     varmeta (some-> sym resolve meta)
                     highlighted-line (when varmeta
                                        (- (.getLineNumber frame) (:line varmeta)))]
                 (recur (conj children
                              (expandable-item
                               (ui/row
                                (file-label (.getFileName primaryframe))
                                (line-number-seg (.getLineNumber primaryframe))
                                (namespaced-seg clsname))
                               (source-view (source-of-classname clsname)
                                            highlighted-line)))
                       (nnext frames)))
               ;; (= "invoke" (.getMethodName frame))
               :else
               (let [sym-str (second (re-matches #"([^/]+/(?:[^/]+|/))(?:/.+)?" (repl/demunge clsname)))
                     sym (some-> sym-str symbol)
                     varmeta (some-> sym resolve meta)
                     highlighted-line (when varmeta
                                        (- (.getLineNumber frame) (:line varmeta)))
                     ui (ui/row
                         (file-label (.getFileName frame))
                         (line-number-seg (.getLineNumber frame))
                         (namespaced-seg clsname))]
                 (recur (conj children
                              (if sym
                                (expandable-item
                                 ui (source-view (source-of-sym sym) highlighted-line))
                                ui))
                       (next frames)))
               )
             (some #(= % "clojure.lang.IType") (map #(.getName %) (ancestors (Class/forName clsname))))
             (recur (conj children
                          (ui/row
                           (file-label (.getFileName frame))
                           (line-number-seg (.getLineNumber frame))
                           (ui/padding
                            0 2 (ns-label clsname))
                           (ui/padding
                            0 2 (ui/label (str " " (repl/demunge (.getMethodName frame)))
                                          font-ui fill-text))
                           ))
                    (next frames))
             :else
             (recur (conj children
                          (ui/row
                           (file-label (.getFileName frame))
                           (line-number-seg (.getLineNumber frame))
                           (ui/padding
                            0 2 (ns-label clsname))
                           (ui/padding
                            0 2 (ui/label (str " " (.getMethodName frame))
                                          font-ui fill-text))))
                    (next frames))))
         (seq children))))))



(comment
  (let [trace (first)])
  (ancestors (Class/forName "chic.ui$draw_child"))
  (ancestors chic.ui.layout.Column)
  (repl/demunge (.getName (class (((fn [] (fn [] (fn []))))))))
  (repl/demunge (.getName (ancestors (class (fn a-b [])))))
  #!
  )
