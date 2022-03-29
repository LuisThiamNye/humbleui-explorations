(ns chic.ui.focusable
  (:require
   [chic.focus :as focus]
   [chic.ui :as cui]
   [io.github.humbleui.ui :as ui]))

(defn make [{:keys[focus-node]} child]
  (ui/dynamic
   ctx [focus-manager (:focus-manager ctx)]
   (cui/clickable
    (fn [event]
      (when (:hui.event.mouse-button/is-pressed event)
        (focus/request-focus focus-manager focus-node)))
    child)))
