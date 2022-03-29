(ns chic.ui.icons.material
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]))

(def svg-dir (io/file "/Volumes/House/pclone/material-design-icons/src"))

;; round, filled, twotone, sharp, outlined
(defn find-svg-file [title style size]
  (let [style (if (= "filled" style) "" style)]
    (or
     (some #(when (= (str "materialicons" style) (fs/file-name %))
              (let [f (fs/file % (str size ".svg"))]
                (when-not (fs/exists? f)
                  (throw (ex-info "This icon does not have that size" {:file f})))
                f))
           (fs/list-dir
            (or (some
                 #(when (= title (fs/file-name %))
                    %)
                 (eduction (filter fs/directory?)
                           (mapcat fs/list-dir)
                           (fs/list-dir svg-dir)))
                (throw (ex-info "Icon name does not exist" {:title title})))))
     (throw (ex-info "Could not find icon for that style" {:title title :style style})))))

(comment
  (find-svg-file "folder" "outlined" "20px")

  #!
  )
