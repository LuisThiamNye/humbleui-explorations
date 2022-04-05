(ns chic.ui.svg
  (:require
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]])
  (:import
   [io.github.humbleui.skija Canvas Data]
   (io.github.humbleui.skija.svg SVGLengthContext SVGDOM SVGSVG SVGLengthType)
   [io.github.humbleui.types IPoint Point]
   [java.lang AutoCloseable]))

(deftype+ Svg [^SVGDOM dom]
  IComponent
  (-measure [_ ctx cs]
            (let [root (.getRoot dom)
                  lc (SVGLengthContext. (Point. (:x cs) (:y cs)))]
                (.getIntrinsicSize root lc)))

  (-draw [_ _ctx cs ^Canvas canvas]
         (let [root ^SVGSVG (.getRoot dom)
               lc (SVGLengthContext. (Point. (:width cs) (:height cs)))
               width (.resolve lc (.getWidth root) SVGLengthType/HORIZONTAL)
               height (.resolve lc (.getHeight root) SVGLengthType/VERTICAL)
               xscale (/ (:width cs) width)
               yscale (/ (:height cs) height)
               layer (.save canvas)]
           (.setContainerSize dom (Point. (:x cs) (:y cs)))
           (.scale canvas xscale yscale)
           (.render dom canvas)
           (.restoreToCount canvas layer)))

  (-event [_ _event])

  AutoCloseable
  (close [_]))

#_(defn file->svgdom [f]
  (let [data-bytes(fs/read-all-bytes f)
        data (Data/makeFromBytes data-bytes)
        svgdom (SVGDOM. data)]
    svgdom))

(defn make [^Data data]
  (->Svg (SVGDOM. data)))

(comment





  #!
  )
