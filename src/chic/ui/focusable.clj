(ns chic.ui.focusable
  (:require
   [chic.focus :as focus]
   [io.github.humbleui.ui :as ui]))

(defn make [{:keys[focus-node]} child]
  (ui/dynamic
   ctx [focus-manager (:focus-manager ctx)]
   (ui/clickable
    #(focus/request-focus focus-manager focus-node)
    child)))
