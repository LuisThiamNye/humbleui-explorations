(ns chic.windows
  (:require
   [chic.debug :as debug]
   [chic.error :as error]
   [chic.error.stacktrace :as error.stacktrace]
   [chic.style :as style]
   [chic.ui :as cui]
   [chic.ui.error :as cui.error]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.profile :as profile]
   [io.github.humbleui.ui :as ui]
   [io.github.humbleui.window :as huiwin])
  (:import
   [io.github.humbleui.jwm EventMouseButton EventMouseMove EventMouseScroll
    EventKey EventWindowFocusOut]
   [io.github.humbleui.skija Canvas Font Paint]
   [io.github.humbleui.types IPoint]))

(defonce *windows (atom {}))

(declare make)

(defn make-render-error-window [{:keys [throwable] :as opts}]
  (let [screen (last (hui/screens))
        scale (:scale screen)
        area (:work-area screen)
        width (* 600 scale)
        height (int (* 0.9 (:height area)))
        x (-> (:width area) (- width) (/ 2) (+ (:x area)))
        y (-> (:height area) (- height) (/ 2) (+ (:y area)))]
    (doto
      (make
       {:id (random-uuid)
        :build-app-root (fn [] (cui/dyncomp (error/build-render-error-window-root opts)))
        :on-close (fn [])})
      (huiwin/set-title (str "Render error: " throwable))
      (huiwin/set-window-size width height)
      (huiwin/set-window-position x y)
      (huiwin/set-visible true))))

(defn on-event-handler [{:keys [*app-root window-obj *ctx]} event]
  (try
    (let [app-root @*app-root
          make-evt (fn [m] (merge (assoc m :chic.ui/component-pos (IPoint. 0 0)
                                         :chic.ui/component-rect (huiwin/content-rect window-obj))
                                  @*ctx))
          changed? (condp instance? event
                     EventMouseMove
                     (let [pos (IPoint. (.getX ^EventMouseMove event) (.getY ^EventMouseMove event))
                           event (make-evt {:hui/event :hui/mouse-move
                                            :chic.ui/mouse-win-pos pos
                                            :hui.event/pos pos})]
                       (swap! *ctx assoc :chic.ui/mouse-win-pos pos)
                       (ui/event app-root event))

                     EventMouseButton
                     (let [event (make-evt
                                  {:hui/event :hui/mouse-button
                                   :event event
                                   :hui.event.mouse-button/is-pressed (.isPressed ^EventMouseButton event)})]
                       (ui/event app-root event))

                     EventMouseScroll
                     (ui/event app-root
                               (make-evt {:hui/event :hui/mouse-scroll
                                          :hui.event.mouse-scroll/dx (.getDeltaX ^EventMouseScroll event)
                                          :hui.event.mouse-scroll/dy (.getDeltaY ^EventMouseScroll event)}))

                     EventWindowFocusOut
                     (do ;;(vreset! *pressed-keys #{})
                       false)

                     EventKey
                     (ui/event app-root
                               (make-evt {:hui/event (if (.isPressed ^EventKey event) :hui/key-down :hui/key-up)
                                          :hui.event.key/key (.getName (.getKey ^EventKey event))
                                          :eventkey event}))

                     nil)]
      (when changed?
        (huiwin/request-frame window-obj)))
    (catch Throwable e
      (debug/println-main (pr-str e)))))

(defn remount-window [window]
  (vreset!
   (:*app-root window)
   ((:build-app-root window)))
  (doseq [window (vals @*windows)]
    (huiwin/request-frame (:window-obj window))))

(def error-view
  (let [font-ui (Font. style/face-code-default (float 13))
        fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))]
    (ui/dynamic
      ctx [{:keys [*ui-error] :as window} (:chic/current-window ctx)
           error @*ui-error]
      (ui/vscrollbar
       (ui/vscroll
        (ui/column (ui/padding
                    5 5
                    (ui/column
                     (ui/clickable
                      #(remount-window window)
                      (ui/fill (doto (Paint.) (.setColor (unchecked-int 0x11000000)))
                               (ui/halign
                                0.5 (ui/padding 20 5 (ui/label "Reload window" font-ui fill-text)))))
                     (ui/padding 5 5 (ui/label (str error) font-ui fill-text))
                     (cui.error/bound-errors
                      (cuilay/column
                       (cui/dyncomp (error.stacktrace/stack-trace-view error))
                       (cui/dyncomp (error/full-error-view-of error))))))))))))

(defn on-paint-handler [{:keys [*app-root window-obj *ctx *ui-error] :as w} ^Canvas canvas]
  (.clear canvas (unchecked-int 0xFFF6F6F6))
  (let [bounds (huiwin/content-rect window-obj)
        ctx (assoc @*ctx :scale (huiwin/scale window-obj)
                   :chic/current-window w
                   :chic.ui/component-rect (huiwin/content-rect window-obj)
                   :chic.ui/component-pos (IPoint. 0 0)
                   :chic.ui/make-render-error-window make-render-error-window)]
    (profile/reset)
    ;; (profile/measure "frame")
    (try (ui/draw @*app-root ctx bounds canvas)
         (catch Throwable e
           (vreset! *ui-error e)
           (vreset! *app-root error-view)
           (ui/draw @*app-root ctx bounds canvas)))
    (profile/log)))

(defn make [{:keys [id on-close *app-root on-paint on-event *ctx build-app-root] :as opts}]
  (let [on-paint (or on-paint #'on-paint-handler)
        on-event (or on-event #'on-event-handler)
        *app-root (or *app-root (volatile! nil))
        w {:id id
           :*app-root *app-root
           :*ui-error (volatile! nil)
           :build-app-root build-app-root
           :*ctx (or *ctx (atom {}))
           :window-obj (huiwin/make
                        {:on-close #(do (swap! *windows dissoc id)
                                        (on-close))
                         :on-paint #(on-paint (get @*windows id) %2)
                         :on-event #(on-event (get @*windows id) %2)})}]
    (vreset! *app-root (build-app-root))
    (swap! *windows assoc id w)
    (:window-obj w)))

(defn request-frame [{:keys [window-obj]}]
  (hui/doui (huiwin/request-frame window-obj)))
