(ns chic.demo
  (:require
   [chic.cljbwr :as cljbwr]
   [chic.demo :as demo]
   [chic.depview :as depview]
   [chic.filebwr :as filebwr]
   [chic.ui :as cui]
   [chic.ui.layout :as cuilay]
   [chic.ui.text-input :as text-input]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Paint]))

(def *state (atom {:selected-tab :home}))

(defn basic-view []
  (ui/dynamic
    ctx [{:keys [font-ui fill-text]} ctx
         {:keys [selected-tab]} @*state]
    (cuilay/column
     (ui/fill
      (doto (Paint.) (.setColor (unchecked-int 0xFFb0b0b0)))
      (cuilay/row
       (for [tab [:home :namespaces :browser :cljbrowser]]
         [:stretch 1
          (cui/clickable
           (fn [event]
             (when (:hui.event.mouse-button/is-pressed event)
               (swap! *state assoc :selected-tab tab)))
           (ui/padding
            1(ui/fill
              (doto (Paint.) (.setColor (if (= selected-tab tab)
                                          (unchecked-int 0xFFeeeeee)
                                          (unchecked-int 0xFFd0d0d0))))
              (ui/padding
               5 8
               (ui/label (name tab) font-ui fill-text)))))])))
     [:stretch 1
      (case selected-tab
        :home (cuilay/halign
               0.5 (cuilay/valign
                    0.5 (cuilay/column
                         (ui/label "Hello world" font-ui fill-text)
                         (ui/gap 0 30)
                         (ui/fill fill-text
                                  (ui/padding
                                   1
                                   (ui/fill
                                    (doto (Paint.) (.setColor (unchecked-int 0xFFFFFFFF)))
                                    (ui/width
                                     300 (cui/dyncomp (text-input/make))))))
                         (ui/gap 0 30)
                         (let [*x (atom false)]
                           (ui/dynamic
                             _ [x @*x]
                             (when x
                               (reset! *x false)
                               (/ 0))
                             (ui/clickable
                              (fn [] (reset! *x true))
                             (ui/fill
                              (doto (Paint.) (.setColor (unchecked-int 0xFFC0C0E0)))
                              (ui/padding
                               20 (ui/label "Throw a UI exception" font-ui fill-text)))))))))
        :browser (cui/dyncomp (filebwr/basic-view))
        :cljbrowser (cui/dyncomp (cljbwr/basic-view))
        :namespaces (cui/dyncomp (depview/basic-view)))])))
