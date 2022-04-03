(ns chic.windows
  (:require
   [chic.debug :as debug]
   [chic.ui.event :as uievt]
   [io.github.humbleui.paint :as huipaint]
   [chic.protocols]
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
    EventKey EventWindowFocusOut App]
   [io.github.humbleui.skija Canvas Font Paint]
   [io.github.humbleui.types IPoint]))

(defn request-frame [{:keys [window-obj]}]
  {:pre [(instance? io.github.humbleui.jwm.Window window-obj)]}
  (if (App/_onUIThread) ;; doui causes freeze if already on UI
    (huiwin/request-frame window-obj)
    (hui/doui
     (huiwin/request-frame window-obj))))

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

(defn window-app-rect [window-obj]
  (cui/rect-with-wh (huiwin/content-rect window-obj)))

(defn on-event-handler [{:keys [*app-root window-obj *ctx] :as win} event]
  (try
    (profile/reset "event")
    (profile/measure
     "event"
     (let [app-root @*app-root
           make-evt (fn [m] (merge (assoc m :chic.ui/component-rect (window-app-rect window-obj))
                                   @*ctx))
           changed? (condp instance? event
                      EventMouseMove
                      (let [pos (IPoint. (.getX ^EventMouseMove event) (.getY ^EventMouseMove event))
                            event (make-evt {:hui/event :hui/mouse-move
                                             :chic.ui/mouse-win-pos pos
                                                             ;; :hui.event/pos pos
                                             })]
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
         (vswap! (:*profiling win) assoc :event-triggers-change-time (System/nanoTime))
         (huiwin/request-frame window-obj))))
    (catch Throwable e
      (debug/println-main (pr-str e)))
    (finally
      #_(profile/log "event"))))

(defn remount-window [window]
  (vreset!
   (:*app-root window)
   ((:build-app-root window)))
  (doseq [window (vals @*windows)]
    (huiwin/request-frame (:window-obj window))))

(defn error-view []
  (ui/dynamic
    ctx [{:keys [scale]} ctx]
    (let [font-ui (Font. style/face-code-default (float (* scale 13)))
         fill-text (huipaint/fill 0xFF000000)]
     (ui/with-context
       {:font-ui font-ui
        :fill-text fill-text}
       (ui/dynamic
         ctx [{:keys [*ui-error] :as window} (:chic/current-window ctx)
              ui-error @*ui-error]
         (let [{:keys [throwable bitmap]} ui-error]
           (ui/fill
            (huipaint/fill 0xFFF6E6E6)
            (cuilay/vscrollbar
             (cuilay/vscroll
              (cuilay/column
               (cuilay/padding
                5 5
                (cuilay/column
                 (cui/clickable
                  (uievt/on-primary-down (fn [_] (remount-window window)))
                  (ui/fill (huipaint/fill 0x11000000)
                           (cuilay/halign
                            0.5 (cuilay/padding 20 5 (ui/label "Reload window" font-ui fill-text)))))
                 (cuilay/padding 5 5 (ui/label (str throwable) font-ui fill-text))
                 (cui.error/bound-errors
                  (cuilay/column
                   (when bitmap
                     (cui/dyncomp(error/partial-canvas-preview bitmap)))
                   (cui/dyncomp (error.stacktrace/stack-trace-view throwable))
                   (cui/dyncomp (error/full-error-view-of throwable))))))))))))))))

(defn on-paint-handler [{:keys [*app-root window-obj *ctx *ui-error] :as w} ^Canvas canvas]
  (.clear canvas (unchecked-int 0xFFF6F6F6))
  (let [bounds (window-app-rect window-obj)
        ctx (assoc @*ctx :scale (or 2 (huiwin/scale window-obj))
                   :chic/current-window w
                   :chic.ui/component-rect bounds
                   :chic.ui/window-content-bounds bounds
                   :chic.error/make-render-error-window make-render-error-window)]
    (profile/reset)
    #_(vswap! (:*profiling w) assoc :paint-start-time (System/nanoTime))
    (profile/measure
     "frame"
     (try (ui/draw @*app-root ctx bounds canvas)
          (catch Throwable e
            (vreset! *ui-error {:bitmap (error/canvas->bitmap canvas bounds)
                                :throwable e
                                :method :draw
                                :ctx ctx
                                :cs bounds})
            (.clear canvas (unchecked-int 0xFFF6F6F6))
            (vreset! *app-root (cui/dyncomp (error-view)))
            (ui/draw @*app-root ctx bounds canvas))))
    #_(vswap! (:*profiling w) assoc :paint-done-time (System/nanoTime))
    #_(let [{:keys [paint-start-time event-triggers-change-time paint-done-time]} @(:*profiling w)]
        (chic.debug/println-main
         "(ms)"
         "change->paint:" (/ (float (- paint-start-time event-triggers-change-time)) 1000000)
         "paint->done: " (/ (float (- paint-done-time paint-start-time)) 1000000)))
    #_(profile/log)))

(defn make [{:keys [id on-close *app-root on-paint on-event *ctx build-app-root] :as opts}]
  (let [on-paint (or on-paint #'on-paint-handler)
        on-event (or on-event #'on-event-handler)
        *app-root (or *app-root (volatile! nil))
        w {:id id
           :*app-root *app-root
           :*ui-error (volatile! nil)
           :*profiling (volatile! nil)
           :build-app-root build-app-root
           :*ctx (or *ctx (atom {}))
           :window-obj (huiwin/make
                        {:on-close #(do (swap! *windows dissoc id)
                                        (on-close))
                         :on-paint #(on-paint (get @*windows id) %2)
                         :on-event #(on-event (get @*windows id) %2)})}
        w (with-meta w
            {`chic.protocols/request-frame #'request-frame})]
    (vreset! *app-root (build-app-root))
    (swap! *windows assoc id w)
    (:window-obj w)))

(comment
  (remount-window (val (first @*windows)))
  (:window-obj (second (vals @*windows)))
  (:window-obj (first (vals @*windows)))

  #!
  )
