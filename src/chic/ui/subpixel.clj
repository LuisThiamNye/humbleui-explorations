(ns chic.ui.subpixel
  (:require
   [clojure.math :as math]
   [chic.ui :as cui]
   [chic.ui.text :as cui.text]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.ui :as ui]
   [chic.text-editor.line :as line])
  (:import
   [java.lang AutoCloseable]
   [io.github.humbleui.types IPoint IRect Point Rect RRect]
   [io.github.humbleui.skija Canvas Font FontMetrics Paint TextLine]
   [io.github.humbleui.skija.shaper Shaper ShapingOptions]))

(defn event-propagate
  ([event child child-rect]
   (event-propagate event child child-rect child-rect))
  ([event child child-rect offset]
   (when (and child child-rect)
     (let [pos    (:hui.event/pos event)
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

(deftype+ SubpixelRow [children ^:mut child-rects]
  IComponent
  (-measure [_ ctx cs]
            (reduce (fn [{:keys [width height]} child]
                      (let [child-size (cui/measure-child child ctx cs {:x width :y 0})]
                        (Point.
                         (+ width (:width child-size))
                         (max height (:height child-size)))))
                    (Point. 0 0)
                    (keep #(nth % 2) children)))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [known (for [[mode _ child] children]
                       (when (= :hug mode)
                         (cui/measure-child child ctx cs)))
               space (- (:width cs) (transduce (keep :width) + 0 known))
               stretch (transduce (keep (fn [[mode value _]] (when (= :stretch mode) value))) + 0 children)
               layer (.save canvas)]
           (loop [width 0
                  rects []
                  known known
                  children children]
             (if-some [[mode value child] (first children)]
               (let [child-size (case mode
                                  :hug (first known)
                                  :stretch (Point. (-> space (/ stretch) (* value)) (:height cs)))]
                 (when child
                   (cui/draw-child child ctx (assoc child-size :height (:height cs)) canvas
                                      {:x width :y 0}))
                 (.translate canvas (:width child-size) 0)
                 (recur
                  (+ width (long (:width child-size)))
                  (conj rects (Rect/makeXYWH width 0 (:width child-size) (:height cs)))
                  (next known)
                  (next children)))
               (set! child-rects rects)))
           (.restoreToCount canvas layer)))

  (-event [_ event]
          (reduce
           (fn [acc [[_ _ child] rect]]
             (hui/eager-or acc (event-propagate event child rect) false))
           false
           (hui/zip children child-rects)))

  AutoCloseable
  (close [_]
         (doseq [[_ _ child] children]
           (ui/child-close child))))

(defn row [& children]
  (cui/dyncomp
    (->SubpixelRow (cuilay/flatten-container children) nil)))

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
