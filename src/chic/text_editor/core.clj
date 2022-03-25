(ns chic.text-editor.core
  (:require
   [chic.text-editor.move]
   [chic.text-editor.delete]
   [chic.text-editor.keybindings :as keybindings]
   [chic.text-editor.cursor]
   [chic.text-editor.insert]
   [chic.text-editor.element]
   [chic.text-editor :as text-editor :refer [->TextEditor]]))

(defn make [opts]
  (let [e (->TextEditor
           (atom {:pos (:pos opts 0)
                  :cursors [{:idx 0
                             :line-id 0}]
                  :target-cursor-x 0
                  ;; :content (:content opts "")
                  :content "ERROR"
                  :line-order [0]
                  :lines-by-id {0 {:content "Hello world!"}}
                  :keydown-handlers [{:id "default"
                                      :handler #(keybindings/handle-keydown-default %1 %2)}]
                  :face-default (:face-default opts)})
           [::focus-node (random-uuid)])]
    (text-editor/enable-normal-mode e)
    e))
