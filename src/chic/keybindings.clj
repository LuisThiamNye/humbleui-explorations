(ns chic.keybindings)

(defn keybinding [up-or-down keyid pred]
  {:direction up-or-down
   :keycode (int keyid)
   :pred pred})
