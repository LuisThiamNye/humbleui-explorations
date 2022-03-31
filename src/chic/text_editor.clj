(ns chic.text-editor
  (:require
   [io.github.humbleui.core :as hui :refer [deftype+]]))

(deftype+ TextEditor [^clojure.lang.Atom state ^:mut focus-node])

(defprotocol PTextEditor_Move
  (move-up [_])
  (move-down [_])
  (move-forwards [_])
  (move-backwards [_]))

(defprotocol PTextEditor_Element
  (element [_]))

(defprotocol PTextEditor_Modes
  (insert-mode? [_])
  (enable-normal-mode [_])
  (enable-insert-mode [_]))

(defprotocol PTextEditor_Insert
  (insert-str [_ s]))

(defprotocol PTextEditor_Delete
  (delete-up-to [_ p])
  (delete-between [_ p1 p2])
  (delete-backwards [_ n])
  (delete-forwards [_ n]))

(defprotocol PTextEditor_Pos
  (at-beginning? [_])
  (at-end? [_])
  (line-end-pos [_])
  (line-start-pos [_]))
