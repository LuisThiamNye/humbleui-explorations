(ns chic.protocols)

(defprotocol PWindow
  :extend-via-metadata true
  (request-frame [_]))
