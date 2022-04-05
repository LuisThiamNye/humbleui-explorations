(ns chic.ui.event
  (:import
   (io.github.humbleui.jwm MouseButton EventMouseButton)))

(defn on-primary-down [handler]
  (fn [event]
    (when (and (:hui.event.mouse-button/is-pressed event)
               (identical? MouseButton/PRIMARY (.getButton ^EventMouseButton (:event event))))
     (handler event))))

(defn mouse-button [event]
  (.getButton ^EventMouseButton (:event event)))
