(ns chic.ui.icons.material
  (:require
   [babashka.fs :as fs]
   [chic.util :as util]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [taoensso.encore :as enc])
  (:import
   (io.github.humbleui.skija Data)))

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

(def icon-name->category
  (into {}
        (map (fn [s]
               (let [[_ category icon] (re-matches #"(.+)::(.+)" s)]
                 [icon category])))
        (keys (json/read-str (slurp "https://raw.githubusercontent.com/google/material-design-icons/master/update/current_versions.json")))))

(def *svg-bytes-cache (atom {}))

(def svg-bytes
  (enc/memoize
   (fn [title style size]
     (util/url->bytes
      (str "https://raw.githubusercontent.com/google/material-design-icons/master/src/"
           (icon-name->category title)
           "/" title
           "/materialicons" (if (= "filled" style) "" style)
           "/" size ".svg")))))

(defn svg-data [title style size]
  #_(str "https://github.com/google/material-design-icons/tree/master/font/MaterialIcons"
       (case style
         "filled" ""
         "round" "Round"
         "outlined" "Outlined"
         "sharp" "Sharp"
         "twotone" "TwoTone")
       "-Regular.otf")
  (Data/makeFromBytes (svg-bytes title style size)))

(comment
  (svg-data "folder" "outlined" "24px")

  #!
  )
