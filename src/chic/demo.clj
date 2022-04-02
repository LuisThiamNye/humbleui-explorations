(ns chic.demo
  (:require
   [chic.ui.event :as uievt]
   [chic.cljbwr :as cljbwr]
   [chic.depview :as depview]
   [io.github.humbleui.paint :as huipaint]
   [chic.filebwr :as filebwr]
   [chic.ui.error :as cui.error]
   [chic.ui :as cui]
   [chic.ui.layout :as cuilay]
   [chic.ui.text-input :as text-input]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Paint]))

(def *state (atom {:selected-tab :home}))

(defn error-button [mode]
  (let [*x (atom false)
        perform-error (fn [] (reset! *x false) (/ 0))]
    (ui/dynamic
     ctx [x @*x
          {:keys [font-ui fill-text]} ctx]
      (when (and x (= :measure mode))
        (perform-error))
      (cui/on-draw
       (fn [_ _ _] (when x (perform-error)))
       (cui/clickable
        (fn [event] (when (:hui.event.mouse-button/is-pressed event)
                      (reset! *x true)))
        (ui/fill
         (doto (Paint.) (.setColor (unchecked-int 0xFFC0C0E0)))
         (cuilay/padding
          20 (ui/label "Throw a UI exception" font-ui fill-text))))))))

(defn layout-view []
  #_(cui/clickable
   (fn [_])
   (ui/dynamic
     ctx [{:hui/keys [hovered? active?]} ctx]
     (prn active?)
     (ui/fill (huipaint/fill (cond
                               active? 0xFF308000
                               hovered? 0xFF60A040
                               :else 0xFFb0b0a0))
              (ui/gap 10 10))))
  (let [a #(ui/fill (huipaint/fill 0xFF0090FF)
                   (ui/gap 10 100))
        b #(ui/fill (huipaint/fill 0xFF3000FF)
                    (ui/gap 10 100))]
    (cuilay/column
     (ui/gap 150 50)
     (cuilay/height
      150
      (cuilay/vscrollbar
       (cuilay/vscroll
        (cuilay/size-dependent
         (fn [cs]
           ;; (prn cs)
           (cuilay/column
           (a) (b) (a) (b ) (a)))))))
     [:stretch 1 (ui/gap 0 10)])))

(defn basic-view []
  (cui/dyncomp (layout-view))
  (ui/dynamic
    ctx [{:keys [font-ui fill-text]} ctx
         {:keys [selected-tab]} @*state]
    (cuilay/column
     (ui/fill
      (huipaint/fill 0xFFb0b0b0)
      (cuilay/row
       (for [tab [:home :namespaces :browser :cljbrowser]]
         [:stretch 1
          (cui/clickable
           (uievt/on-primary-down (fn [_] (swap! *state assoc :selected-tab tab)))
           (ui/dynamic
             ctx [{:hui/keys [hovered?]} ctx]
             (cuilay/padding
              1 (ui/fill
                 (if (= selected-tab tab)
                   (huipaint/fill 0xFFeeeeee)
                   (huipaint/fill (cond
                                    hovered? 0xFFc0c0c0
                                    :else 0xFFd0d0d0)))
                 (cuilay/padding
                  5 8
                  (ui/label (name tab) font-ui fill-text))))))])))
     [:stretch 1
      (case selected-tab
        :home (cuilay/vscrollbar
               (cuilay/vscroll
                (cuilay/halign
                 0.5 (cuilay/column
                      (ui/gap 0 20)
                      (ui/label "Hello world" font-ui fill-text)
                      (ui/gap 0 30)
                      (ui/fill fill-text
                               (cuilay/padding
                                1
                                (ui/fill
                                 (huipaint/fill 0xFFFFFFFF)
                                 (cuilay/width
                                  300 (cui/dyncomp (text-input/make))))))
                      (ui/gap 0 30)
                      (cui/dyncomp (error-button :draw))
                      (ui/gap 0 30)
                      (cuilay/padding 4 (ui/label "Error boundary: -draw" font-ui fill-text))
                      (cui.error/bound-errors
                       (ui/fill
                        (huipaint/fill 0xFF000000)
                        (cuilay/padding
                         1 (ui/fill (huipaint/fill 0xFFFFFFFF)
                                    (cuilay/padding
                                     10 (cuilay/column
                                         (cuilay/padding
                                          0 4 (ui/label "We're able to draw this" font-ui fill-text))
                                         (cui/dyncomp (error-button :draw))
                                         (cuilay/padding
                                          0 4 (ui/label "But not this" font-ui fill-text))))))))
                      (ui/gap 0 20)
                      (cuilay/padding 4 (ui/label "Error boundary: -measure" font-ui fill-text))
                      (cui.error/bound-errors
                       (ui/fill
                        (huipaint/fill 0xFF000000)
                        (cuilay/padding
                         1 (ui/fill (huipaint/fill 0xFFFFFFFF)
                                    (cuilay/padding
                                     10 (cui/dyncomp (error-button :measure)))))))
                      (ui/gap 0 30)))))
        :browser (cui/dyncomp (filebwr/basic-view))
        :cljbrowser (cui/dyncomp (cljbwr/basic-view))
        :namespaces (cui/dyncomp (depview/basic-view)))])))
