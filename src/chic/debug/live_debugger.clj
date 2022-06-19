(ns chic.debug.live-debugger
  (:require
   [clojure.string :as str]
   [chic.ui :as cui]
   [chic.style :as style]
   [chic.error.stacktrace :as error.stacktrace]
   [chic.digger.inspector.stack :refer [expandable-item]]
   [chic.debug :as debug]
   [chic.ui.error :as cui.error]
   [chic.ui.event :as uievt]
   [clojure.reflect :as reflect]
   [chic.digger :as digger]
   [chic.digger.inspector.throwable :as inspector.throwable]
   [chic.digger.inspector.thread-shapshot :as inspector.thread-shapshot]
   [io.github.humbleui.profile :as profile]
   [chic.ui.icons.material :as maticons]
   [io.github.humbleui.paint :as huipaint]
   [chic.ui.layout :as cuilay]
   [chic.ui.svg :as ui.svg]
   [chic.windows :as windows]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.ui :as ui]
   [io.github.humbleui.window :as huiwin]
   [taoensso.encore :as enc])
  (:import
   (java.lang.reflect Method Field)
   [io.github.humbleui.skija Paint Shader Canvas]
   [io.github.humbleui.types IPoint Point Rect]))

(defn source-view [^com.sun.jdi.StackFrame frame]
  (let [^com.sun.jdi.Location location (.location frame)]
    (cui/dynamic
     ctx [{:keys [font-ui font-code fill-text]} ctx]
     (cuilay/column
      (ui/fill (huipaint/fill 0x40000000) (ui/gap 0 1))
      (let [source-path (try (.sourcePath location)
                                (catch Exception _ nil))]
        (let [clsname (.name (.declaringType location))
              sym (error.stacktrace/classname->sym clsname)
              varmeta (some-> sym resolve meta)
              line-number (.lineNumber location)#_(.getLineNumber ^StackTraceElement (:trace-element frame))]
          (cuilay/column
           (cuilay/halign
            1 (cuilay/row
               (cui/clickable
                (uievt/on-primary-down
                 (fn [evt] (cui/emit evt [[:chic.digger.inspector/inspect-result
                                           location]])))
                (cuilay/padding
                 4 (ui/label (str line-number "  " (or source-path (:file varmeta))) font-ui fill-text)))
               (cui/clickable
                (uievt/on-primary-down
                 (fn [evt] (cui/emit evt [[:chic.digger.inspector/inspect-result
                                           frame]])))
                (ui/fill (huipaint/fill 0x30000000)
                         (cuilay/padding
                          4 (ui/label "Frame" font-ui fill-text))))
               #_(ui/gap 2 0)
               #_(cui/clickable
                (uievt/on-primary-down
                 (fn [evt] (cui/emit evt [[:chic.digger.inspector/inspect-result
                                           (:trace-element frame)]])))
                (ui/fill (huipaint/fill 0x30000000)
                         (cuilay/padding
                          4 (ui/label "Trace" font-ui fill-text))))
               (ui/gap 2 0)
               (cui/clickable
                (uievt/on-primary-down
                 (fn [evt] (cui/emit evt [[:chic.digger.inspector/inspect-result
                                           (.visibleVariables frame)]])))
                (ui/fill (huipaint/fill 0x30000000)
                         (cuilay/padding
                          4 (ui/label "Vars" font-ui fill-text))))))
           (cuilay/hscroll
            (error.stacktrace/source-view
             (error.stacktrace/source-of-classname clsname)
             (when varmeta
               (- line-number (:line varmeta))))))))
      (ui/fill (huipaint/fill 0x40000000) (ui/gap 0 1))))))

(defn make [info]
  (let [thread-ref ^com.sun.jdi.ThreadReference (:thread-ref info)]
    (cui/dynamic
     ctx [{:keys [font-ui font-code fill-text]} ctx]
     (cuilay/column
      (for [^com.sun.jdi.StackFrame frame (.frames thread-ref)]
        (let [location (.location frame)
              clsname (.name (.method location))]
          (expandable-item
           (cuilay/row
            (cuilay/width
             130 (cuilay/halign
                  1 (cuilay/padding
                     5 3
                     (ui/label (str clsname) font-code fill-text))))
            (cuilay/padding
             5 3
             (cui/dyncomp (error.stacktrace/class-name-view (.name (.declaringType location))))))
           #(cui/dyncomp (source-view frame)))))))))
