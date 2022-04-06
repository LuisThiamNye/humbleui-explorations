(ns chic.util)

(defn url->bytes [url]
  (with-open [^java.io.InputStream input-stream (.openStream (java.net.URL. url))]
    (loop [bs []]
      (let [b (.read input-stream)]
        (if (== -1 b)
          (byte-array bs)
          (recur (conj bs b)))))))

(defn assoc-if-not= [m k v]
  (if (= (get m k) v)
    m
    (assoc m k v)))
