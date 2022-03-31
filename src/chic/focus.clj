(ns chic.focus)

;; (deftype+ FocusNode [])

(defn request-focus [manager focus-node]
  (reset! (:*primary-focus manager) focus-node))

(defn has-focus? [manager focus-node]
  (= focus-node @(:*primary-focus manager)))

(defn new-manager []
  {:*primary-focus (atom nil)})
