(ns chic.style
  (:require
   [babashka.fs :as fs]
   [chic.util :as util])
  (:import
   [io.github.humbleui.skija FontMgr FontStyle Typeface Data]))

(def input-font-path (let [s "/Volumes/Carbonator/csync/fonts/Input-Font/Input_Fonts/InputSans/InputSansCondensed/InputSansCondensed-Regular.ttf"]
                       (when (fs/exists? s) s)))

(def ^Typeface face-default
  (.matchFamiliesStyle (FontMgr/getDefault)
                       (into-array String ["Roboto Slab", #_".SF NS" ;; slow?
                                           "Helvetica Neue", "Arial"])
                       FontStyle/NORMAL))

(def ^Typeface face-code-default
  (if false;input-font-path
    (Typeface/makeFromFile input-font-path)
    (Typeface/makeFromData
     (Data/makeFromBytes
      (util/url->bytes (re-find #"https:.+\.ttf"
                           (slurp "https://fonts.googleapis.com/css2?family=Fira+Code&display=swap")))))))
