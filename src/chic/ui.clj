(ns chic.ui
  (:require
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.ui :as ui])
  (:import
   [java.lang AutoCloseable]
   [io.github.humbleui.types IPoint IRect Point Rect RRect]
   [io.github.humbleui.skija Bitmap Canvas Font FontMetrics Paint TextLine ImageInfo ColorAlphaType]
   [io.github.humbleui.skija.shaper Shaper ShapingOptions]))

(defn point-in-component? [{offset :chic.ui/component-pos
                            cs :chic.ui/component-rect} pos]
  {:pre [(some? pos)]}
  (and (<= (:x offset) (:x pos))
       (<= (:x pos) (+ (:x offset) (:width cs)))
       (<= (:y offset) (:y pos))
       (<= (:y pos) (+ (:y offset) (:height cs)))))

(defn component-relative-pos [{offset :chic.ui/component-pos} pos]
  (IPoint. (unchecked-subtract-int (:x pos) (:x offset))
           (unchecked-subtract-int (:y pos) (:y offset))))

(alter-var-root
 #'ui/event-propagate
 (fn [_]
   (fn event-propagate
     ([event child child-rect]
      (event-propagate event child child-rect child-rect))
     ([event child child-rect offset]
      (when (and child child-rect)
        (let [pos (:hui.event/pos event)
              compos (:chic.ui/component-pos event)
              event' (cond
                       (nil? pos)
                       event

                       (not (.contains ^IRect child-rect pos))
                       (dissoc event :hui.event/pos)

                       (= 0 (:x offset) (:y offset))
                       event

                       :else
                       (assoc event :hui.event/pos
                              (IPoint. (- (:x pos) (:x offset)) (- (:y pos) (:y offset)))))
              event' (cond-> event' offset
                             (assoc :chic.ui/component-pos
                                    (IPoint. (unchecked-add-int (:x compos) (:x offset))
                                             (unchecked-add-int (:y compos) (:y offset)))))]
          (huip/-event child (assoc event' :chic.ui/component-rect child-rect))))))))

(defn child-ctx
  ([ctx child cs]
   (child-ctx ctx child cs nil))
  ([ctx child cs offset]
   (if (and child offset cs)
     (let [pos (:chic.ui/component-pos ctx)]
       (assoc ctx :chic.ui/component-pos
              (IPoint. (unchecked-add-int (:x pos) (:x offset))
                       (unchecked-add-int (:y pos) (:y offset)))))
     ctx)))

(defn measure-child
  ([child ctx cs]
   (huip/-measure child (child-ctx ctx child cs) cs))
  ([child ctx cs offset]
   (huip/-measure child (child-ctx ctx child cs offset) cs)))

(defn measure-child-unbounded
  ([child ctx]
   (let [cs (IPoint. Integer/MAX_VALUE Integer/MAX_VALUE)]
     (huip/-measure child (child-ctx ctx child cs) cs))))

(defn draw-child
  ([child ctx cs canvas]
   (huip/-draw child (child-ctx ctx child cs) cs canvas))
  ([child ctx cs canvas offset]
   (huip/-draw child (child-ctx ctx child cs offset) cs canvas)))

(defmacro dyncomp [child]
  `(ui/dynamic _ctx# [_# (deref (var ~(first child)))]
               ~child))

(deftype+ Clickable [on-event child ^:mut child-rect ^:mut hovered? ^:mut pressed?]
  IComponent
  (-measure [_ ctx cs]
            (measure-child child ctx cs))

  (-draw [_ ctx cs canvas]
         (set! child-rect (IRect/makeXYWH 0 0 (:width cs) (:height cs)))
         (set! hovered? (when-let [pos (:chic.ui/mouse-win-pos ctx)]
                          (point-in-component? ctx pos)))
         (let [ctx' (cond-> ctx
                      hovered? (assoc :hui/hovered? true)
                      (and pressed? hovered?) (assoc :hui/active? true))]
           (draw-child child ctx' cs canvas)))

  (-event [_ event]
          (hui/eager-or
           (when (or (= :hui/mouse-button (:hui/event event))
                     (= :hui/mouse-move (:hui/event event)))
             (let [hovered?' (point-in-component? event (:chic.ui/mouse-win-pos event))]
               (when (not= hovered? hovered?')
                 (set! hovered? hovered?')
                 true)))
           (when (= :hui/mouse-button (:hui/event event))
             (let [pressed?' (if (:hui.event.mouse-button/is-pressed event)
                               (if hovered?
                                 (do (on-event event)
                                     true)
                                 (do false))
                               (do
                                 (when pressed?
                                   (on-event (assoc event :hovered? hovered?)))
                                 false))]
               (when (not= pressed? pressed?')
                 (set! pressed? pressed?')
                 true)))
           (ui/event-propagate event child child-rect)))

  AutoCloseable
  (close [_]
         (ui/child-close child)))

(defn clickable [on-event child]
  (dyncomp
   (->Clickable on-event child nil false false)))

(deftype+ EventListener [on-event child]
  IComponent
  (-measure [_ ctx cs] (measure-child child ctx cs))

  (-draw [_ ctx cs canvas] (draw-child child ctx cs canvas))

  (-event [_ event] (hui/eager-or
                     (on-event event)
                     (huip/-event child event)))
  AutoCloseable
  (close [_] (ui/child-close child)))

(defn on-event [handler child]
  (->EventListener handler child))

(deftype+ MouseMoveListener [on-event child]
  IComponent
  (-measure [_ ctx cs] (measure-child child ctx cs))

  (-draw [_ ctx cs canvas] (draw-child child ctx cs canvas))

  (-event [_ event]
          (hui/eager-or
           (when (= :hui/mouse-move (:hui/event event))
             (do
               (on-event event)
               true))
           (huip/-event child event)))

  AutoCloseable
  (close [_]
         (ui/child-close child)))

(defn on-mouse-move [on-event child]
  (dyncomp
   (->MouseMoveListener on-event child)))

(deftype+ BeforeDrawHook [on-draw child]
  IComponent
  (-measure [_ ctx cs] (measure-child child ctx cs))
  (-draw [_ ctx cs ^Canvas canvas]
         (on-draw ctx cs canvas)
         (draw-child child ctx cs canvas))
  (-event [_ event] (huip/-event child event))
  AutoCloseable
  (close [_] (ui/child-close child)))

(defn on-draw [on-draw child]
  (->BeforeDrawHook on-draw child))
