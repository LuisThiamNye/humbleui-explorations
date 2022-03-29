(ns chic.ui.error
  (:require
   [chic.error :as error]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.ui :as ui]
   [chic.windows :as windows]
   [chic.style :as style]
   [io.github.humbleui.window :as huiwin]
   [chic.ui.layout :as cuilay]
   [chic.ui :as cui])
  (:import
   [java.lang AutoCloseable]
   [io.github.humbleui.types IPoint IRect Point Rect RRect]
   [io.github.humbleui.skija Bitmap Canvas Font FontMetrics Paint TextLine ImageInfo ColorAlphaType]
   [io.github.humbleui.skija.shaper Shaper ShapingOptions]))

(defn make-render-error-window [{:keys [throwable] :as opts}]
  (let [screen (last (hui/screens))
        scale (:scale screen)
        area (:work-area screen)
        width (* 600 scale)
        height (int (* 0.9 (:height area)))
        x (-> (:width area) (- width) (/ 2) (+ (:x area)))
        y (-> (:height area) (- height) (/ 2) (+ (:y area)))]
    (doto
      (windows/make
       {:id (random-uuid)
        :build-app-root (fn [] (cui/dyncomp (error/build-render-error-window-root opts)))
        :on-close (fn [])})
      (huiwin/set-title (str "Render error: " throwable))
      (huiwin/set-window-size width height)
      (huiwin/set-window-position x y)
      (huiwin/set-visible true))))

(defn boundary-draw-error-view [e ctx cs bitmap]
  (let [font-ui (Font. style/face-code-default (float 12))
        fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))]
    (ui/fill
     (doto (Paint.) (.setColor (unchecked-int 0xFFFFD0D0)))
     (ui/clip
      (cuilay/column
       (ui/clickable
        (fn [] (make-render-error-window {:throwable e :bitmap bitmap}))
        (ui/padding
         2 4 (ui/label (str "Render error: " e) font-ui fill-text)))
       (error/full-error-view-of e))))))

(deftype+ ErrorBoundary [child ^:mut draw-error-child]
  error/PErrorBoundary
  (reset-error-boundary [_] (set! draw-error-child nil))

  IComponent
  (-measure [self ctx cs]
            (let [ctx (assoc ctx :error-boundary self)]
              (cui/measure-child (or draw-error-child child) ctx cs)))

  (-draw [self ctx cs canvas]
         (let [errctx (assoc ctx :error-boundary self)]
           (when-not draw-error-child
             (let [layer (.save canvas)
                   e (try (cui/draw-child child ctx cs canvas)
                          nil
                          (catch Throwable e e)
                          (finally (.restoreToCount canvas layer)))]
               (when e
                 (let [imageinfo (ImageInfo/makeN32Premul (:width cs) (:height cs))
                       bitmap (doto (Bitmap.) (.allocPixels imageinfo))
                       offset (:chic.ui/component-pos ctx)]
                   (.readPixels canvas bitmap (:x offset) (:y offset))
                   (.setImmutable bitmap)
                   (set! draw-error-child
                         (doto (boundary-draw-error-view e errctx cs bitmap)
                           (cui/measure-child errctx cs)))))))
           (when draw-error-child
             (cui/draw-child draw-error-child errctx cs canvas))))

  (-event [_ event] (huip/-event (or draw-error-child child) event))

  AutoCloseable
  (close [_]
         (ui/child-close child)
         (some-> draw-error-child ui/child-close)))

(defmacro bound-errors [child]
  `(try (cui/dyncomp (->ErrorBoundary ~child nil))
        (catch Throwable e#
          (cui/dyncomp (error/error-view-of e#)))))
