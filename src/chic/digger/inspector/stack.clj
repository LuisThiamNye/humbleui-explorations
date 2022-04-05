(ns chic.digger.inspector.stack
  (:require
   [clojure.string :as str]
   [chic.ui :as cui]
   [chic.error.stacktrace :as error.stacktrace]
   [chic.ui.event :as uievt]
   [clojure.reflect :as reflect]
   [io.github.humbleui.profile :as profile]
   [chic.ui.icons.material :as maticons]
   [io.github.humbleui.paint :as huipaint]
   [chic.ui.layout :as cuilay]
   [chic.ui.svg :as ui.svg]
   [chic.windows :as windows]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.ui :as ui]
   [taoensso.encore :as enc])
  (:import
   (java.lang.reflect Method Field)
   [io.github.humbleui.skija Paint Shader Canvas]
   [io.github.humbleui.types IPoint Point Rect]))

(defn expandable-item [toggle contentsf]
  (let [*expanded? (volatile! false)]
    (cuilay/column
     (cui/clickable
      (fn [event]
        (when (:hui.event.mouse-button/is-pressed event)
          (vswap! *expanded? not)))
      toggle)
     (ui/dynamic ctx [expanded? @*expanded?]
       (if expanded?
         (contentsf)
         (ui/gap 0 0))))))
