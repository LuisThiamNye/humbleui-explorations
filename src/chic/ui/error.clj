(ns chic.ui.error
  (:require
   [chic.error :as error]
   [clojure.string :as str]
   [chic.style :as style]
   [chic.ui :as cui]
   [chic.ui.layout :as cuilay]
   [chic.ui.icons.material :as maticons]
   [chic.ui.svg :as ui.svg]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.paint :as huipaint]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Bitmap Font Paint ImageInfo Canvas]
   [java.lang AutoCloseable]))

(defn boundary-error-view* [{:keys [throwable] :as info}]
  (ui/dynamic
    ctx [{:keys [scale fill-text]} ctx]
    (when fill-text
      (ui/with-context
       {:font-ui (Font. style/face-default (float (* scale 10)))}
        (ui/dynamic
          ctx [{:keys [font-ui]
                eb :error-boundary} ctx]
          (when font-ui
            (ui/fill
            (huipaint/fill 0xFFFFD0D0)
            (ui/clip
             (cuilay/column
              (ui/dynamic
                ctx [make-render-error-window (:chic.error/make-render-error-window ctx)]
                (cui/clickable
                 (fn [event]
                   (when (:hui.event.mouse-button/is-pressed event)
                     (make-render-error-window (assoc info :error-boundary eb))))
                 (ui/dynamic
                   ctx [{:hui/keys [hovered?]} ctx]
                   (ui/fill
                    (huipaint/fill (if hovered? 0x11000000 0x00000000))
                    (cuilay/row
                     (cuilay/padding
                      1
                      (cuilay/height
                       14 (cuilay/width #(:height %)
                                        (ui.svg/make (maticons/svg-data "open_in_new" "filled" "24px")))))
                     [:stretch 1
                      (cuilay/valign
                       0.5 (cuilay/padding
                            2 4 (cuilay/halign
                                 0.5 (ui/label (str throwable) font-ui fill-text))))])))))
              (ui/fill (huipaint/fill 0x30000000)
                       (ui/gap 0 1))
              (cui/clickable
               (fn [event]
                 (when (:hui.event.mouse-button/is-pressed event)
                   (error/reset-error-boundary eb)))
               (ui/dynamic
                 ctx [{:hui/keys [hovered?]} ctx]
                 (ui/fill
                  (huipaint/fill (if hovered? 0x11000000 0x00000000))
                  (cuilay/row
                   (cuilay/padding
                    1 (cuilay/height
                       14 (cuilay/width #(:height %)
                                        (ui.svg/make (maticons/svg-data "refresh" "filled" "24px")))))
                   [:stretch 1
                    (cuilay/halign
                     0.5 (cuilay/padding 2 4 (ui/label "Reload boundary" font-ui fill-text)))]))))
              (ui/fill (huipaint/fill 0x30000000)
                       (ui/gap 0 1))
              (when-not (instance? java.lang.StackOverflowError throwable)
                (for [sfe (.getStackTrace ^Throwable throwable)]
                  (cuilay/padding
                   0 3
                   (ui/label (pr-str sfe) font-ui fill-text)))))))))))))

(defn boundary-error-view [info]
  (cui/dyncomp (boundary-error-view* info)))

(deftype+ ErrorBoundary [child ^:mut size ^:mut measure-error-child ^:mut draw-error-child]
  error/PErrorBoundary
  (reset-error-boundary [_]
                        (set! draw-error-child nil)
                        (set! measure-error-child nil))

  IComponent
  (-measure [self ctx cs]
            (let [errctx (assoc ctx :error-boundary self)]
              (when-not (or draw-error-child measure-error-child)
                (let [*size (volatile! size)
                      e (try (vreset! *size (cui/measure-child child ctx cs))
                             nil
                             (catch Throwable e e))]
                  (set! size @*size)
                  (when e
                    (set! measure-error-child (boundary-error-view {:throwable e
                                                                    :method :measure
                                                                    :ctx errctx
                                                                    :cs cs}))
                    #_(set! size (cui/measure-child measure-error-child ctx cs)))))
              (or size cs)))

  (-draw [self ctx cs ^Canvas canvas]
         (let [errctx (assoc ctx :error-boundary self)]
           (when-not draw-error-child
             (let [layer (.save canvas)
                   e (try (cui/draw-child child ctx cs canvas)
                          nil
                          (catch Throwable e e)
                          (finally (.restoreToCount canvas layer)))]
               (when e
                 (set! draw-error-child
                       (doto (boundary-error-view {:bitmap (error/canvas->bitmap canvas cs)
                                                   :throwable e
                                                   :method :draw
                                                   :ctx errctx
                                                   :cs cs})
                         (cui/measure-child errctx cs))))))
           (cond
             measure-error-child
             (cui/draw-child measure-error-child errctx cs canvas)
             draw-error-child
             (cui/draw-child draw-error-child errctx cs canvas))))

  (-event [_ event] (huip/-event (or measure-error-child draw-error-child child) event))

  AutoCloseable
  (close [_]
         (ui/child-close child)
         (some-> draw-error-child ui/child-close)))

(defn bound-errors* [child]
  (cui/dyncomp (->ErrorBoundary child nil nil nil)))

(defmacro bound-errors [child]
  `(try (bound-errors* ~child)
        (catch Throwable e#
          (cui/dyncomp (error/error-view-of e#)))))
