(ns chic.style
  (:import
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventMouseScroll
    EventKey Window EventWindowFocusOut]
   [io.github.humbleui.skija Canvas FontMgr FontStyle Typeface Font Paint PaintMode]
   [io.github.humbleui.types IPoint]))

(def ^Typeface face-default
  (.matchFamiliesStyle (FontMgr/getDefault)
                       (into-array String ["Roboto Slab", ".SF NS", "Helvetica Neue", "Arial"])
                       FontStyle/NORMAL))

(def ^Typeface face-code-default
  (Typeface/makeFromFile "/Volumes/Carbonator/csync/fonts/Input-Font/Input_Fonts/InputSans/InputSansCondensed/InputSansCondensed-Regular.ttf"))
