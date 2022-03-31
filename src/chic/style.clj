(ns chic.style
  (:import
   [io.github.humbleui.skija FontMgr FontStyle Typeface]))

(def ^Typeface face-default
  (.matchFamiliesStyle (FontMgr/getDefault)
                       (into-array String ["Roboto Slab", ".SF NS", "Helvetica Neue", "Arial"])
                       FontStyle/NORMAL))

(def ^Typeface face-code-default
  (Typeface/makeFromFile "/Volumes/Carbonator/csync/fonts/Input-Font/Input_Fonts/InputSans/InputSansCondensed/InputSansCondensed-Regular.ttf"))
