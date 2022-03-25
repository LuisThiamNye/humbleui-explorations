(ns chic.text-editor.id)

(defn new-line-id [state]
  (inc (reduce max Long/MIN_VALUE (:line-order state))))
