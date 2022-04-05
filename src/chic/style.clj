(ns chic.style
  (:require
   [babashka.fs :as fs]
   [chic.util :as util])
  (:import
   [io.github.humbleui.skija FontMgr FontStyle Typeface Data Font Paint]))

(def input-font-path (let [s "/Volumes/Carbonator/csync/fonts/Input-Font/Input_Fonts/InputSans/InputSansCondensed/InputSansCondensed-Regular.ttf"]
                       (when (fs/exists? s) s)))

(def ^Typeface face-default
  (.matchFamiliesStyle (FontMgr/getDefault)
                       (into-array String ["Roboto Slab", #_".SF NS" ;; slow?
                                           "Helvetica Neue", "Arial"])
                       FontStyle/NORMAL))

(def ^Typeface face-code-default
  (if input-font-path
    (Typeface/makeFromFile input-font-path)
    (Typeface/makeFromData
     (Data/makeFromBytes
      (util/url->bytes (re-find #"https:.+\.ttf"
                                (slurp "https://fonts.googleapis.com/css2?family=Fira+Code&display=swap")))))))

(defn context-default [{:keys [scale]}]
  (let [font-ui (Font. face-default (float (* 14 scale)))
        fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
        font-code (Font. face-code-default (float (* 14 scale)))]
    {:face-ui face-default
     :font-ui font-ui
     :face-code face-code-default
     :font-code font-code
     :fill-text fill-text}))
