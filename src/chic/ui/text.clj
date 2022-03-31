(ns chic.ui.text
  (:require
   [chic.ui :as cui]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]])
  (:import
   [io.github.humbleui.skija Canvas Font FontMetrics Paint TextLine]
   [io.github.humbleui.skija.shaper Shaper ShapingOptions]
   [io.github.humbleui.types IPoint]
   [java.lang AutoCloseable]))

;; (defprotocol PDrawableTextSpan
;;   (draw-text [_ canvas]))

;; (deftype+ DrawableTextSpan [^String text ^Font font ^Paint paint ^TextLine line ^FontMetrics metrics]
;;   PDrawableTextSpan
;;   (draw-text
;;    [_ ^Canvas canvas]
;;    (.drawTextLine canvas line 0 (Math/ceil (.getCapHeight metrics)) paint)))

;; (defn drawable-text-span []
;;   (->DrawableTextSpan ))

;; (defn multi-span-label [spans]
;;   (cui/dyncomp
;;    (->MultiSpanLabel spans)))

(deftype+ RightAlignedLabel [^String text ^Font font ^Paint paint ^TextLine line ^FontMetrics metrics]
  IComponent
  (-measure [_ _ctx _cs]
            (IPoint.
             (Math/ceil (.getWidth line))
             (Math/ceil (.getCapHeight metrics))))

  (-draw [_ _ctx cs ^Canvas canvas]
         (.drawTextLine canvas line (unchecked-subtract (:width cs) (.getWidth line))
                        (Math/ceil (.getCapHeight metrics)) paint))

  (-event [_ _event])

  AutoCloseable
  (close [_]
         #_(.close line))) ; TODO

(def ^:private ^Shaper shaper (Shaper/makeShapeDontWrapOrReorder))

(defn text-line [^String text ^Font font & [features]]
  (let [opts (reduce #(.withFeatures ^ShapingOptions %1 ^String %2) ShapingOptions/DEFAULT features)
        line (.shapeLine shaper text font ^ShapingOptions opts)]
    line))

(defn right-aligned-label
  [^String text ^Font font ^Paint paint & features]
  (let [line (text-line text font features)]
    (cui/dyncomp
     (->RightAlignedLabel text font paint line (.getMetrics ^Font font)))))

(defn right-aligned-label-from-textline [^TextLine textline ^String text ^Font font ^Paint paint]
  (cui/dyncomp
   (->RightAlignedLabel text font paint textline (.getMetrics ^Font font))))
