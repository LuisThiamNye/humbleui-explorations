(ns chic.text-editor.insert
  (:require
   [chic.text-editor :as text-editor :refer [PTextEditor_Insert]]
   [chic.text-editor.cursor :as cursor]
   [chic.text-editor.id :as id]
   [clojure.string :as str]
   [taoensso.encore :as enc])
  (:import
   (chic.text_editor TextEditor)))

(defn insert-str-in-line [{:keys [content] :as line} idx s]
  (-> line
      (assoc :content
             (str (subs content 0 idx)
                  s
                  (subs content idx)))))

(defn create-lines-before-from-strs [sm line-id new-line-strs]
  (let [new-lines (eduction (map (fn [s]
                                   {:content s}))
                            new-line-strs)
        line-order (:line-order sm)
        line-idx (loop [idx 0]
                   (if (= line-id (nth line-order idx))
                     idx
                     (recur (inc idx))))
        [sm new-line-order] (reduce (fn [[sm lo] l]
                                      (let [new-id (id/new-line-id sm)]
                                        [(assoc-in sm [:lines-by-id new-id] l)
                                         (conj! lo new-id)]))
                                    [sm (enc/into! (transient []) (subvec line-order 0 line-idx))]
                                    new-lines)]
    (assoc sm :line-order
           (persistent! (enc/into! new-line-order (subvec line-order line-idx))))))

(extend-type TextEditor
  PTextEditor_Insert
  (insert-str [{:keys [state]} s]
    (swap! state
           (fn [{:keys [lines-by-id cursors] :as sm}]
             (let [split-lines (str/split-lines s)
                   lines-to-insert (into (if (zero? (count split-lines)) [""] split-lines)
                                         (repeat (count (re-find #"\n+$" s)) ""))
                   new-lines-count (dec (count lines-to-insert))
                   new-lines (subvec lines-to-insert 0 new-lines-count)
                   str-to-append (peek lines-to-insert)]
               (reduce (fn [sm {:keys [idx line-id i]}]
                         (let [final-line-idx (if (zero? new-lines-count)
                                                idx
                                                0)]
                           (-> sm
                              (cond-> (pos? new-lines-count)
                                (create-lines-before-from-strs
                                 line-id (update new-lines 0
                                                 (fn [ls]
                                                   (str (subs (:content (get lines-by-id line-id)) 0 idx) ls)))))
                              (update-in [:lines-by-id line-id]
                                         (fn [{:keys [content] :as line}]
                                           (-> line
                                               (cond-> (pos? new-lines-count)
                                                 (assoc :content (subs content idx (count content))))
                                               (insert-str-in-line final-line-idx str-to-append))))
                              (assoc-in [:cursors i :idx] (+ final-line-idx (count str-to-append))))))
                       sm
                       (cursor/cursors-indexed sm)))))))


(comment

  #!
  )
