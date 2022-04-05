(ns chic.error
  (:require
   [chic.error.stacktrace :as error.stacktrace]
   [chic.protocols :as protocols]
   [chic.style :as style]
   [io.github.humbleui.paint :as huipaint]
   [chic.ui :as cui]
   [chic.ui.layout :as cuilay]
   [clojure.string :as str]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Bitmap Image Font Paint ImageInfo Canvas]
   [io.github.humbleui.types Rect]))

(defprotocol PErrorBoundary
  (reset-error-boundary [_]))

(defn canvas->bitmap [^Canvas canvas cs]
  (let [imageinfo (ImageInfo/makeN32Premul (:width cs) (:height cs))
        bitmap (doto (Bitmap.) (.allocPixels imageinfo))]
    (.readPixels canvas bitmap (:x cs) (:y cs))
    (.setImmutable bitmap)))

(defn full-error-view-of [error]
  (ui/dynamic
    ctx [eb (:error-boundary ctx)
         {:keys [fill-text font-ui]} ctx]
    (cuilay/padding
     5 5
     (cuilay/column
      (when eb
        (cui/clickable
         (fn [event]
           (when (:hui.event.mouse-button/is-pressed event)
             (reset-error-boundary eb)))
         (ui/fill (huipaint/fill 0x11000000)
                  (cuilay/halign
                   0.5 (cuilay/padding 20 5 (ui/label "Reload boundary" font-ui fill-text))))))
      (when-not (instance? java.lang.StackOverflowError error)
        (for [l (str/split-lines (pr-str error))]
          (cuilay/padding
           0 3
           (ui/label l font-ui fill-text))))
      (ui/gap 0 50)))))

(defn error-view-of [error]
  (ui/fill (huipaint/fill 0xA0FFFFFF)
           (cuilay/vscrollbar
            (cuilay/vscroll
             (full-error-view-of error)))))

(defn partial-canvas-preview [^Bitmap bitmap]
  (let [imageinfo ^ImageInfo (.getImageInfo bitmap)
        ratio (/ (.getHeight imageinfo) (.getWidth imageinfo))]
    (ui/dynamic
      ctx [{:keys [scale]} ctx]
      (cuilay/padding
      15 (cuilay/row
          [:stretch 1 (ui/gap 0 0)]
          (cuilay/height
           #(* ratio (min (/ (.getWidth imageinfo) scale) (:width %)))
           (ui/custom-ui
            (.getWidth imageinfo) (.getHeight imageinfo)
            {:on-paint (fn [^Canvas canvas width height]
                         (.drawRectShadow canvas (Rect/makeXYWH 0 0 width height)
                                          0. 0. 10. 0. (unchecked-int 0x40000000))
                         (.scale canvas
                                 (min 1 (float (/ width (.getWidth imageinfo))))
                                 (min 1 (float (/ height (.getHeight imageinfo)))))
                         (.drawImage canvas (Image/makeFromBitmap bitmap) 0. 0.)
                         (.scale canvas 1 1))}))
          [:stretch 1 (ui/gap 0 0)])))))

(defn build-render-error-window-root [{:keys [throwable bitmap method error-boundary] :as info}]
  (ui/dynamic
    ctx [{:keys [scale]} ctx]
    (ui/with-context
      {:font-ui (Font. style/face-code-default (float (* scale 12)))
       :fill-text (huipaint/fill 0xFF000000)
       :font-code (Font. style/face-code-default (float (* 12 scale)))}
      (ui/dynamic
        ctx [{:keys [fill-text font-ui]} ctx]
        (ui/fill
         (huipaint/fill 0xFFFFFFFF)
         (cuilay/vscrollbar
          (cuilay/vscroll
           (cuilay/column
            (when error-boundary
              (cuilay/padding
               5 5
               (cui/clickable
                (fn [event]
                  (when (:hui.event.mouse-button/is-pressed event)
                    (reset-error-boundary error-boundary)
                    (protocols/request-frame (:chic/current-window (:ctx info)))))
                (ui/fill (huipaint/fill 0x11000000)
                         (cuilay/halign
                          0.5 (cuilay/padding 20 5 (ui/label "Reload boundary" font-ui fill-text)))))))
            (cuilay/padding
             2 5 (ui/label (str throwable) font-ui fill-text))
            (when method
              (cuilay/padding
               2 5 (ui/label (str "(During " (case method :measure "measurement" :draw "draw")
                                  " phase.)") font-ui fill-text)))
            (when bitmap
              (cui/dyncomp (partial-canvas-preview bitmap)))
            (cui/dyncomp (error.stacktrace/stack-trace-view throwable))
            (cui/dyncomp (full-error-view-of throwable))))))))))
