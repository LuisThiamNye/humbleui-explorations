(ns chic.util)

(defn url->bytes [url]
  (with-open [input-stream (.openStream (java.net.URL. url))]
    (loop [bs []]
      (let [b (.read input-stream)]
        (if (== -1 b)
          (byte-array bs)
          (recur (conj bs b)))))))
