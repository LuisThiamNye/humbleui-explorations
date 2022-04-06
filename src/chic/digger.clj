(ns chic.digger
  (:require
   [chic.ui :as cui]
   [chic.digger.inspector :as inspector]
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
   [io.github.humbleui.skija Paint Shader Canvas]
   [io.github.humbleui.types IPoint Point Rect]))

(defn new-inspector [obj]
  (cui/dyncomp (inspector/inspector obj)))

(defn new-session [obj]
  (atom {:inspectors [(new-inspector obj)]}))

(defn close-session [*state]
  (doseq [ins (:inspectors @*state)]
    (ui/child-close ins)))

(defn make [*state]
  (ui/dynamic
   ctx [{:keys [fill-text font-ui]} ctx]
   (cui/with-bounds
     :digger-bounds
     (cuilay/stack
      (cuilay/valign
       0 (cuilay/halign
          0.5 (cuilay/padding
               0 5 0 0
               (ui/dynamic
                _ [{:keys [inspectors]} @*state]
                (cuilay/row
                 (interpose
                  (ui/gap 5 0)
                  (map (fn [_]
                         (ui/clip-rrect
                          2
                          (ui/fill (huipaint/fill 0xFFc9c9c9)
                                   (ui/gap 10 10))))
                       inspectors)))))))
      (cuilay/hscroll
       (ui/dynamic
        ctx [{:keys [inspectors]} @*state
             {:keys [digger-bounds]} ctx]
        (cuilay/column
         (ui/gap 0 14)
         [:stretch 1
          (cuilay/row
           (if (== 0 (count inspectors))
             (ui/label "<empty>" font-ui fill-text)
             (map-indexed
              (fn [i inspector]
                [:hug nil
                 (cuilay/width
                  (- (/ (:width digger-bounds) 2) 0.01)
                  (cuilay/padding
                   10 (cui/shadow-rect
                       0 0 7 0 0x40000000
                       (cui/on
                        ::inspector/inspect-result
                        (fn [x]
                          (swap! *state assoc :inspectors
                                 (conj (vec (take (inc i) inspectors))
                                       (new-inspector x))))
                        inspector))))])
              inspectors)))])))))))

(defonce *state (new-session nil))

(defn digger-tap-handler [x]
  (close-session *state)
  (reset! *state (new-session (new-inspector x))))

(add-tap #'digger-tap-handler)

(defn basic-view []
  (cui/dyncomp (make *state)))
