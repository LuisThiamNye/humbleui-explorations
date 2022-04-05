(ns chic.digger.inspector.throwable
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
   [taoensso.encore :as enc]))

(defn stack-trace-inspector-view [^Throwable throwable]
  (cui/dyncomp (error.stacktrace/stack-trace-view throwable)))
