(ns chic.text-editor.line)

(defn end-idx [{:keys [content]}]
  {:pre [(string? content)]}
  (unchecked-dec-int (count content)))

(defn next-end-of-big-word-idx [{:keys [content]} idx]
  (let [m (re-matcher #"\S(?:\s|$)" content)]
    (when (.find m (inc idx))
      (.start m))))

(defn next-start-of-big-word-idx [{:keys [content]} idx]
  (let [m (re-matcher #"(?:^|\s)\s*\S" content)]
    (when (.find m (inc idx))
      (unchecked-dec-int (.end m)))))

(defn prev-start-of-big-word-idx [{:keys [content]} idx]
  (loop [i (unchecked-dec-int idx)
         seen-non-ws? false]
    (cond
      (and (or (neg? i) (Character/isWhitespace (nth content i)))
           seen-non-ws?)
      (inc i)
      (neg? i)
      nil
      :else
      (recur (unchecked-dec-int i)
             (or seen-non-ws?
                 (not (Character/isWhitespace (nth content i))))))))

(defn next-end-of-word-idx [{:keys [content] :as line} idx]
  (next-end-of-big-word-idx line idx))

(defn next-start-of-word-idx [{:keys [content] :as line} idx]
  (next-start-of-big-word-idx line idx))

(defn prev-start-of-word-idx [{:keys [content] :as line} idx]
  (prev-start-of-big-word-idx line idx))

(defn line-order-idx [sm line-id]
  (let[line-order (:line-order sm)]
    (loop [idx 0]
     (if (= line-id (nth line-order idx))
       idx
       (recur (inc idx))))))

(defn next-line [sm line-id]
  (nth (:line-order sm)
       (inc (line-order-idx sm line-id))
       nil))

(defn prev-line [sm line-id]
  (nth (:line-order sm)
       (unchecked-dec-int (line-order-idx sm line-id))
       nil))

(comment
  (next-start-of-big-word-idx {:content "abc"} -1)
  #!
  )
