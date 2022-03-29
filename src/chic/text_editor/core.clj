(ns chic.text-editor.core
  (:require
   [clojure.string :as str]
   [chic.text-editor.move]
   [chic.text-editor.delete]
   [chic.text-editor.keybindings :as keybindings]
   [chic.text-editor.cursor]
   [chic.text-editor.insert]
   [chic.text-editor.element]
   [chic.text-editor :as text-editor :refer [->TextEditor]])
  (:import
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventMouseScroll
    EventKey Window EventWindowFocusOut]
   [io.github.humbleui.skija Canvas FontMgr FontStyle Typeface Font Paint PaintMode]
   [io.github.humbleui.types IPoint]))

(defn make [opts]
  (let [lines (str/split-lines (:content opts ""))
        e (->TextEditor
           (atom {:pos (:pos opts 0)
                  :cursors [{:idx 0
                             :line-id 0}]
                  :target-cursor-x 0
                  ;; :content (:content opts "")
                  :line-order (vec (range (count lines)))
                  :lines-by-id (into {} (map-indexed (fn [i x] [i {:content x}]))
                                     lines)
                  :keydown-handlers [{:id "default"
                                      :handler #(keybindings/handle-keydown-default %1 %2)}]
                  :face-default (:face-default opts (.matchFamiliesStyle (FontMgr/getDefault)
                                                                         (into-array String ["Roboto Slab", ".SF NS", "Helvetica Neue", "Arial"])
                                                                         FontStyle/NORMAL))})
           [::focus-node (random-uuid)])]
    (text-editor/enable-normal-mode e)
    e))
