(ns chic.error.stacktrace
  (:require
   [chic.clj.source :as clj.source]
   [taoensso.encore :as enc]
   [io.github.humbleui.paint :as huipaint]
   [chic.debug :as debug]
   [chic.style :as style]
   [chic.ui :as cui]
   [chic.ui.layout :as cuilay]
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
                 (cuilay/column
                  toggle
                  (when expanded?
                    contents))))))

(defn classname->sym [clsn]
  (some-> (re-matches #".+/(?:[^/]+|/)" (repl/demunge clsn)) symbol))

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

(defn stack-trace-segment-view [frames]
  (ui/dynamic
    ctx [{:keys [font-ui fill-text]} ctx]
    (let [file-col-width 110
         text-vpadding 3
         file-label (fn [filename]
                      (cuilay/width
                       file-col-width
                       (cuilay/halign
                        1 (cuilay/row
                           (cuilay/padding
                            0 text-vpadding (ui/label (if (= "NO_SOURCE_FILE" filename)
                                                        "" filename) font-ui fill-text))
                           (ui/gap 5 0)))))
         line-number-seg (fn [n]
                           (ui/clip
                            (cuilay/width
                             40
                             (cuilay/halign
                              1 (cuilay/row
                                 (cuilay/padding
                                  0 text-vpadding (ui/label (str n) font-ui fill-text))
                                 (ui/gap 8 0))))))
          ns-foreign-fill (huipaint/fill 0x80000000)
         ns-fill (doto (Paint.) (.setColor (unchecked-int 0xC0003040)))
         ns-label (fn [clsname]
                    (ui/label clsname font-ui (if (str/starts-with? clsname "chic")
                                                ns-fill ns-foreign-fill)))
         namespaced-seg (fn [clsname]
                          (let [[_ nspart namepart] (re-matches #"(.+/)([^/]+|/)" (repl/demunge clsname))]
                            (cuilay/row
                             (when nspart
                               (cuilay/padding
                                0 text-vpadding (ns-label nspart)))
                             (cuilay/padding
                              0 text-vpadding (ui/label (or namepart clsname) font-ui fill-text)))))
         source-view (fn [content idx]
                       (cuilay/padding
                        7 0 (cuilay/column
                             (for [[i line] (map-indexed #(vector %1 %2) (str/split-lines content))]
                               (ui/fill
                                (doto (Paint.) (.setColor (unchecked-int (if (= i idx)
                                                                           0x50D0a020
                                                                           0x00000000))))
                                (cuilay/padding
                                 0 text-vpadding
                                 (ui/label line font-ui fill-text))))
                             (ui/gap 0 10))))]
     (cuilay/column
      (loop [children []
             frames (seq frames)]
        (if-let [frame (first frames)]
          (let [clsname (:class-name frame)]
            (cond
              (some #(= % "clojure.lang.Fn") (map #(.getName ^Class %) (ancestors (Class/forName clsname))))
              (cond
                (and (= "invokeStatic" (:method-name frame))
                     (= "invoke" (:method-name (second frames))))
                (let [primaryframe (second frames)
                      sym (classname->sym clsname)
                      varmeta (some-> sym resolve meta)
                      highlighted-line (when varmeta
                                         (- (:line-number frame) (:line varmeta)))]
                  (recur (conj children
                               (expandable-item
                                (cuilay/row
                                 (file-label (:file-name primaryframe))
                                 (line-number-seg (:line-number primaryframe))
                                 (namespaced-seg clsname))
                                (source-view (source-of-classname clsname)
                                             highlighted-line)))
                         (nnext frames)))
                ;; (= "invoke" (:method-name frame))
                :else
                (let [sym-str (second (re-matches #"([^/]+/(?:[^/]+|/))(?:/.+)?" (repl/demunge clsname)))
                      sym (some-> sym-str symbol)
                      varmeta (some-> sym resolve meta)
                      highlighted-line (when varmeta
                                         (- (:line-number frame) (:line varmeta)))
                      ui (cuilay/row
                          (file-label (:file-name frame))
                          (line-number-seg (:line-number frame))
                          (namespaced-seg clsname))]
                  (recur (conj children
                               (if sym
                                 (expandable-item
                                  ui (source-view (source-of-sym sym) highlighted-line))
                                 ui))
                         (next frames))))
              (some #(= % "clojure.lang.IType") (map #(.getName ^Class %) (ancestors (Class/forName clsname))))
              (recur (conj children
                           (cuilay/row
                            (file-label (:file-name frame))
                            (line-number-seg (:line-number frame))
                            (cuilay/padding
                             0 2 (ns-label clsname))
                            (cuilay/padding
                             0 2 (ui/label (str " " (repl/demunge (:method-name frame)))
                                           font-ui fill-text))))
                     (next frames))
              :else
              (recur (conj children
                           (cuilay/row
                            (file-label (:file-name frame))
                            (line-number-seg (:line-number frame))
                            (cuilay/padding
                             0 2 (ns-label clsname))
                            (cuilay/padding
                             0 2 (ui/label (str " " (:method-name frame))
                                           font-ui fill-text))))
                     (next frames))))
          (seq children)))))))

#_(defn repeated-leading-items [items]
  (loop [init-stack [(first items)]
         repeat-stack []
         i 1]
    (when-let [item (nth items i nil)]
      (if (= init-stack repeat-stack)
          init-stack
          (if (= item (nth init-stack (count repeat-stack)))
            (recur init-stack
                   (conj repeat-stack item)
                   (inc i))
            (if (== 0 (count repeat-stack))
              (recur (conj (into init-stack repeat-stack) item)
                     []
                     (inc i))
              (recur (into init-stack repeat-stack)
                     []
                     i)))))))

(defn repeated-items-info [items]
  (loop [init-stack [(first items)]
         i 1]
    (when-let [item (nth items i nil)]
      (or (when-let [seen-idx (first (keep-indexed #(when (= %2 item) %1) init-stack))]
            (let [seg (subvec init-stack seen-idx (count init-stack))
                  end-idx (+ i (count seg))]
              (when (and (< end-idx (count items))
                         (= seg (subvec items i end-idx)))
                {:segment seg
                 :start-idx seen-idx
                 :end-idx (dec i)})))
          (recur (conj init-stack item) (inc i))))))

(comment
  (repeated-items-info [1 2 3 4 5 3 4 5 0 0 0])
  #!
  )

(defn partition-leading-repeats [segment]
  (fn [rf]
    (let [stack (java.util.ArrayList. (count segment))
          remainder (transient [])
          *done? (volatile! false)]
      (fn
       ([] (rf))
        ([acc]
         (let [remainder (persistent! remainder)
               acc (if (.isEmpty stack)
                     (unreduced (rf acc remainder))
                     (let [v (vec (.toArray stack))]
                       (.clear stack)
                       (unreduced (rf acc (into v remainder)))))]
           (rf acc)))
        ([acc x]
         (if @*done?
           (do (conj! remainder x)
               acc)
           (if (= x (nth segment (.size stack)))
             (do (.add stack x)
                 (if (== (count segment) (.size stack))
                   (let [repeat (vec (.toArray stack))]
                     (.clear stack)
                     (rf acc repeat))
                   acc))
             (do (vreset! *done? true)
                 (enc/into! remainder (.toArray stack))
                 (.clear stack)
                 (conj! remainder x)
                 acc))))))))

(defn stack-trace-view [^Throwable throwable]
  (let [frames (eduction
                (map (fn [^StackTraceElement frame]
                       {:file-name (.getFileName frame)
                        :class-name (.getClassName frame)
                        :method-name (.getMethodName frame)
                        :line-number (.getLineNumber frame)
                        :native-method? (.isNativeMethod frame)}))
                (.getStackTrace throwable))]
    (ui/with-context
      {:font-ui (Font. style/face-code-default (float 12))
       :fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))}
      (if (instance? java.lang.StackOverflowError throwable)
       (let [frames (vec frames)]
         (if-let [{:keys [segment start-idx end-idx]} (repeated-items-info frames)]
           (let [[nrepeats remainder]
                 (reduce (fn [acc x]
                           [(inc (nth acc 0)) x])
                         [-1 []] ;; last element is non-repeated frames
                         (eduction (partition-leading-repeats segment)
                                   (subvec frames (inc end-idx))))]
             (ui/dynamic
               ctx [{:keys [font-ui fill-text]} ctx]
               (cuilay/column
                (stack-trace-segment-view (subvec frames 0 start-idx))
                (cuilay/padding
                 8 (ui/label (str "Repeated segment:") font-ui fill-text))
                (stack-trace-segment-view segment)
                (cuilay/padding
                 8 (ui/label (str "Seen again " nrepeats " times") font-ui fill-text))
                (stack-trace-segment-view remainder))))
           (stack-trace-segment-view frames)))
       (stack-trace-segment-view frames)))))

(comment


  (:start-idx (repeated-items-info --fs))
  (:end-idx (repeated-items-info --fs))

  (subvec --fs 0 6)
  (:segment (repeated-items-info --fs))
  (subvec --fs 11 16)
  (last --fs)

  (let [trace (first)])
  (ancestors (Class/forName "chic.ui$draw_child"))
  (ancestors chic.ui.layout.Column)
  (repl/demunge (.getName (class (((fn [] (fn [] (fn []))))))))
  (repl/demunge (.getName (ancestors (class (fn a-b [])))))
  #!
  )
