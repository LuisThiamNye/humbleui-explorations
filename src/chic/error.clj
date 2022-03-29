(ns chic.error
  (:require
   [chic.debug :as debug]
   [chic.error.stacktrace :as error.stacktrace]
   [chic.ui :as cui]
   [chic.ui.layout :as cuilay]
   [clojure.repl :as repl]
   [clojure.string :as str]
   [chic.style :as style]
   [chic.focus :as focus]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as paint]
   [io.github.humbleui.profile :as profile]
   [io.github.humbleui.window :as huiwin]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventMouseScroll
    EventKey Window EventWindowFocusOut]
   [io.github.humbleui.skija Canvas FontMgr FontStyle Image Typeface Font Paint PaintMode]
   [io.github.humbleui.types IPoint Rect]))

(defprotocol PErrorBoundary
  (reset-error-boundary [_]))

(defn full-error-view-of [error]
  (let [font-ui (Font. style/face-code-default (float 13))
        fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))]
    (ui/dynamic
     ctx [eb (:error-boundary ctx)]
     (ui/padding
      5 5
      (ui/column
       (when eb
         (cui/clickable
          (fn [event]
            (when (:hui.event.mouse-button/is-pressed event)
              (reset-error-boundary eb)))
          (ui/fill (doto (Paint.) (.setColor (unchecked-int 0x11000000)))
                   (ui/halign
                    0.5 (ui/padding 20 5 (ui/label "Reload" font-ui fill-text))))))
       (for [l (str/split-lines (pr-str error))]
         (ui/padding
          0 3
          (ui/label l font-ui fill-text)))
       (ui/gap 0 50))))))

(defn error-view-of [error]
  (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xA0FFFFFF)))
           (ui/vscrollbar
            (ui/vscroll
             (full-error-view-of error)))))

(defn build-render-error-window-root [{:keys [throwable bitmap]}]
  (let [font-ui (Font. style/face-code-default (float 12))
        fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
        imageinfo (.getImageInfo bitmap)]
    (ui/fill
     (doto (Paint.) (.setColor (unchecked-int 0xFFFFFFFF)))
     (cuilay/vscrollbar
      (cuilay/vscroll
       (cuilay/column
        (ui/padding
         2 4 (ui/label (str throwable) font-ui fill-text))
        (ui/halign
         0.5 (ui/padding
              0 10 (ui/dynamic
                    ctx [offset (:chic.ui/component-pos ctx)]
                    (ui/custom-ui
                     (.getWidth imageinfo) (.getHeight imageinfo)
                     {:on-paint (fn [canvas width height]
                                  (.drawRectShadow canvas (Rect/makeXYWH 0 0 width height)
                                                   0. 0. 10. 0. (unchecked-int 0x40000000))
                                  (.drawImage canvas (Image/makeFromBitmap bitmap) 0. 0.))}))))
        (cui/dyncomp (error.stacktrace/stack-trace-view throwable))
        (cui/dyncomp (full-error-view-of throwable))))))))
