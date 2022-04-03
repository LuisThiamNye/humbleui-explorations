(ns chic.ui.subpixel
  (:require
   [chic.ui :as cui]
   [chic.ui.layout :as cuilay]
   [chic.ui.text :as cui.text]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Canvas Font FontMetrics Paint TextLine]
   [io.github.humbleui.types Point Rect IPoint]
   [java.lang AutoCloseable]))

(defn event-propagate
  ([event child child-rect]
   (event-propagate event child child-rect child-rect))
  ([event child child-rect offset]
   (when (and child child-rect)
     (let [pos (:hui.event/pos event)
           event' (cond
                    (nil? pos)
                    event

                    (not (.contains ^Rect child-rect (:x pos) (:y pos)))
                    (dissoc event :hui.event/pos)

                    (= 0 (:x offset) (:y offset))
                    event

                    :else
                    (assoc event :hui.event/pos
                           (Point. (- (:x pos) (:x offset)) (- (:y pos) (:y offset)))))]
       (huip/-event child event')))))

(defn row [& children]
  (cui/dyncomp
   (cuilay/->Row (cuilay/flatten-container children) nil true)))

(defrecord SubpixelGap [width height]
  IComponent
  (-measure [_ ctx cs]
    (let [{:keys [scale]} ctx]
      (Point. (* scale width) (* scale height))))
  (-draw [_ ctx cs canvas])
  (-event [_ event]))

(defn gap [width height]
  (->SubpixelGap width height))

(deftype+ SubpixelLabel [^String text ^Font font ^Paint paint ^TextLine line ^FontMetrics metrics]
  IComponent
  (-measure [_ _ctx _cs]
            (Point. (.getWidth line) (.getCapHeight metrics)))

  (-draw [_ _ctx _cs ^Canvas canvas]
         (.drawTextLine canvas line 0 (.getCapHeight metrics) paint))

  (-event [_ _event])

  AutoCloseable
  (close [_]
         #_(.close line)))

(defn label
  [^String text ^Font font ^Paint paint & features]
  (let [line (cui.text/text-line text font features)]
    (->SubpixelLabel text font paint line (.getMetrics ^Font font))))

(defn label-from-textline [^TextLine textline ^String text ^Font font ^Paint paint]
  (->SubpixelLabel text font paint textline (.getMetrics ^Font font)))

(deftype+ SubpixelBoundary [child]
  IComponent
  (-measure [_ ctx cs]
            (let [child-size(huip/-measure child ctx (Rect/makeXYWH (:x cs) (:y cs) (:width cs) (:height cs)))]
              (IPoint. (Math/ceil (:width child-size)) (Math/ceil (:height child-size)))))
  (-draw [_ ctx cs canvas]
         (huip/-draw child ctx (Rect/makeXYWH (:x cs) (:y cs) (:width cs) (:height cs)) canvas))
  (-event [_ event]
          (let [cs (:chic.ui/component-rect event)]
            (huip/-event child
                         (assoc event :chic.ui/component-rect
                                (Rect/makeXYWH (:x cs) (:y cs) (:width cs) (:height cs))))))
  AutoCloseable
  (close [_] (ui/child-close child)))

(defn subpixel-boundary [child]
  (->SubpixelBoundary child))
