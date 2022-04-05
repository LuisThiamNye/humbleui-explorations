(ns chic.digger.inspector.thread-shapshot
  (:require
   [clojure.string :as str]
   [chic.ui :as cui]
   [chic.error.stacktrace :as error.stacktrace]
   [chic.digger.inspector.stack :refer [expandable-item]]
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

(defn frame-variables-inspector-view [variables]
  (ui/dynamic
    ctx [{:keys [font-ui font-code fill-text]} ctx]
    (cuilay/column
     (for [[^com.sun.jdi.LocalVariable variable value] variables]
       (cuilay/row
        [:stretch 1
         (ui/clip
          (cuilay/padding
           4 (ui/label (.name variable) font-ui fill-text)))]
        [:stretch 2
         (cuilay/padding
          4 (ui/label (str value) font-ui fill-text))])))))

(defn source-view [frame]
  (let [^com.sun.jdi.Location location (:location frame)]
    (ui/dynamic
     ctx [{:keys [font-ui font-code fill-text]} ctx]
     (cuilay/column
      (ui/fill (huipaint/fill 0x40000000) (ui/gap 0 1))
      (let [source-path (try (.sourcePath location)
                                (catch Exception _ nil))]
        (let [clsname (.name (.declaringType location))
              sym (error.stacktrace/classname->sym clsname)
              varmeta (some-> sym resolve meta)
              line-number (.getLineNumber ^StackTraceElement (:trace-element frame))]
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
               (ui/gap 2 0)
               (cui/clickable
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
                                           (:visible-variables frame)]])))
                (ui/fill (huipaint/fill 0x30000000)
                         (cuilay/padding
                          4 (ui/label "Vars" font-ui fill-text))))))
           (cuilay/hscroll
            (error.stacktrace/source-view
             (error.stacktrace/source-of-classname clsname)
             (when varmeta
               (- line-number (:line varmeta))))))))
      (ui/fill (huipaint/fill 0x40000000) (ui/gap 0 1))))))

(defn frames-inspector-view [frames]
  (ui/dynamic
   ctx [{:keys [font-ui font-code fill-text]} ctx]
   (cuilay/column
    (for [{:keys [^com.sun.jdi.Location location] :as frame} frames]
      (let [clsname (.name (.method location))]
        (expandable-item
         (cuilay/row
          #_(cuilay/width
             40 (let [line-number (.lineNumber location)]
                  (if (== -1 line-number)
                    (ui/gap 0 0)
                    (cuilay/halign
                     1 (cuilay/padding
                        5 3 (ui/label (str line-number) font-code fill-text))))))
          (cuilay/width
           130 (cuilay/halign
                1 (cuilay/padding
                   5 3
                   (ui/label (str clsname) font-code fill-text))))
          (cuilay/padding
           5 3
           (cui/dyncomp (error.stacktrace/class-name-view (.name (.declaringType location))))
           #_(ui/label (str (.method location)) font-code fill-text))
          #_(cuilay/padding
             5 3 (ui/label (str (try (.sourceName location)
                                     (catch Exception _ ""))) font-code fill-text)))
         #(cui/dyncomp (source-view frame))))))))

(defn inspector-view [_])
