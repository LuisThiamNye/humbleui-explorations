(ns chic.debug)

(defonce main-out *out*)

(defn println-main [& args]
  (doseq [a args]
    (.write main-out (str a)))
  (.write main-out "\n")
  (.flush main-out))
