(ns chic.ui
  (:require
   [clojure.pprint :as pp]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Canvas]
   [io.github.humbleui.types IPoint IRect Rect Point]
   [java.lang AutoCloseable]))
#_#_#_#_(defmethod print-method IRect [o w]
          (print-simple (str "#IRect["
                             (.getLeft o)
                             " " (.getTop o)
                             " " (.getRight o)
                             " " (.getBottom o) "]") w))

      (defmethod print-method Rect [o w]
        (print-simple (str "#Rect["
                           (.getLeft o)
                           " " (.getTop o)
                           " " (.getRight o)
                           " " (.getBottom o) "]") w))

    (defmethod print-method IPoint [o w]
      #_(print-simple (str "#IPoint[" (.getX o) " " (.getY o) "]") w))

  (defmethod print-method Point [o w]
    (print-simple (str "#Point[" (.getX o) " " (.getY o) "]") w))

(defn assert-good-cs [cs]
  (when (or (instance? Point cs) (instance? IPoint cs)
            ;; (neg? (:right cs))
            ;; (neg? (:x cs))
            ;; (neg? (:bottom cs))
            ;; (neg? (:y cs))
            (neg? (:width cs)) (neg? (:height cs)))
    (throw (doto (ex-info "bad cs" {:cs cs}) pp/pprint)))
  cs)
;; (remove-method print-method IPoint)
;; (hui/doui (prn 4))

(defn offset-lt [rect dx dy]
  (assert-good-cs rect)
  (assert-good-cs
   (cond
     (instance? IRect rect)
     (-> ^IRect rect
         (.withLeft (+ (.getLeft ^IRect rect) dx))
         (.withTop (+ (.getTop ^IRect rect) dy)))
     (instance? Rect rect)
     (-> ^Rect rect
         (.withLeft (+ (.getLeft ^Rect rect) dx))
         (.withTop (+ (.getTop ^Rect rect) dy)))
     :else
     (IRect/makeXYWH dx dy (:x rect) (:y rect)))))

(defn offset-ltrb [^IRect rect dl dt dr db]
  (assert-good-cs rect)
  (assert-good-cs
   (IRect/makeLTRB
    (+ (.getLeft rect) dl) (+ (.getTop rect) dt)
    (- (.getRight rect) dr) (- (.getBottom rect) db))))

(defn offset-ltrb-float [^Rect rect dl dt dr db]
  (assert-good-cs rect)
  (assert-good-cs
   (Rect/makeLTRB
    (+ (.getLeft rect) dl) (+ (.getTop rect) dt)
    (- (.getRight rect) dr) (- (.getBottom rect) db))))

(defn offset-lw [rect dl width]
  (assert-good-cs rect)
  (assert-good-cs
   (if (instance? IRect rect)
     (let [r (+ (.getLeft ^IRect rect) dl width)]
       (.withRight ^IRect (offset-lt rect dl 0) (min Integer/MAX_VALUE r)))
     (let [r (+ (.getLeft ^Rect rect) dl width)]
      (.withRight ^Rect (offset-lt rect dl 0) (min Integer/MAX_VALUE r))))))

(defn offset-th [rect dt height]
  (assert-good-cs rect)
  (assert-good-cs
   (if (instance? IRect rect)
     (let [b (+ (.getTop ^IRect rect) dt height)]
       (.withBottom (.withTop (.withLeft ^IRect rect (+ (.getLeft ^IRect rect) 0))
                              (+ (.getTop ^IRect rect) dt))
                    (min Integer/MAX_VALUE b)))
     (let [b (+ (.getTop ^Rect rect) dt height)]
      (.withBottom (.withTop (.withLeft ^Rect rect (+ (.getLeft ^Rect rect) 0))
                             (+ (.getTop ^Rect rect) dt))
                   (min Integer/MAX_VALUE b))))))

(defn rect-with-wh
  ([{:keys [width height]}]
   (IRect/makeXYWH 0 0 width height))
  ([rect {:keys [width height]}]
   (assert-good-cs rect)
   (assert-good-cs
    (if (instance? IRect rect)
      (IRect/makeXYWH (.getLeft ^IRect rect) (.getTop ^IRect rect) width height)
      (Rect/makeXYWH (.getLeft ^Rect rect) (.getTop ^Rect rect) width height))))
  ([rect width height]
   (assert-good-cs rect)
   (assert-good-cs
    (if (instance? IRect rect)
      (IRect/makeXYWH (.getLeft ^IRect rect) (.getTop ^IRect rect) width height)
      (Rect/makeXYWH (.getLeft ^Rect rect) (.getTop ^Rect rect) width height)))))

(defn rect-translate [rect dx dy]
  (assert-good-cs rect)
  (assert-good-cs (if (instance? IRect rect)
                    (.offset ^IRect rect dx dy)
                    (.offset ^Rect rect dx dy))))

(defn unbounded-width [rect]
  (assert-good-cs rect)
  (if (instance? IRect rect)
    (-> ^IRect rect
        (.withLeft 0)
        (.withRight Integer/MAX_VALUE))
    (assoc rect :width Integer/MAX_VALUE)))

(defn unbounded-height [rect]
  (assert-good-cs rect)
  (if (instance? IRect rect)
    (-> ^IRect rect
        (.withTop 0)
        (.withBottom Integer/MAX_VALUE))
    (assoc rect :height Integer/MAX_VALUE)))

(defn unbounded-right [rect]
  (assert-good-cs rect)
  (if (instance? IRect rect)
    (-> ^IRect rect
        (.withRight (min Integer/MAX_VALUE
                          (+ (:x rect) Integer/MAX_VALUE))))
    (assoc rect :width Integer/MAX_VALUE)))

(defn unbounded-bottom [rect]
  (assert-good-cs rect)
  (if (instance? IRect rect)
    (-> ^IRect rect
        (.withBottom (min Integer/MAX_VALUE
                          (+ (:y rect) Integer/MAX_VALUE))))
    (assoc rect :height Integer/MAX_VALUE)))

(defn point-in-component? [{rect :chic.ui/component-rect} pos]
  {:pre [(some? pos)]}
  (if (instance? IRect rect)
    (.contains ^IRect rect pos)
    (.contains ^Rect rect pos))
  #_(and (<= (:x rect) (:x pos))
       (<= (:x pos) (+ (:x rect) (:width rect)))
       (<= (:y rect) (:y pos))
       (<= (:y pos) (+ (:y rect) (:height rect)))))

(defn component-relative-pos [{rect :chic.ui/component-rect} pos]
  (IPoint. (unchecked-subtract-int (:x pos) (:x rect))
           (unchecked-subtract-int (:y pos) (:y rect))))

(alter-var-root
 #'ui/event-propagate
 (fn [_]
   (fn event-propagate
     ([event child child-rect]
      (event-propagate event child child-rect child-rect))
     ([event child child-rect offset]
      (when (and child child-rect)
        (let [rect (:chic.ui/component-rect event)
              rect' (IRect/makeXYWH (+ (:x rect) (:x offset))
                                    (+ (:y rect) (:y offset))
                                    (:width child-rect) (:height child-rect))]
          (huip/-event child (assoc event :chic.ui/component-rect rect'))))))))

(defn event-propagate [event child child-rect]
  (when (and child child-rect)
    (huip/-event child (assoc event :chic.ui/component-rect child-rect))))

(defn child-ctx
  ([ctx child cs]
   (assert-good-cs cs)
   (assoc ctx :chic.ui/component-rect cs)
   #_(if (and child offset cs)
       (let [rect (:chic.ui/component-rect ctx)]
         (assoc ctx :chic.ui/component-rect (.offset cs (:x rect) (:y rect))))
       ctx)
   #_(child-ctx ctx child cs nil))
  #_([ctx child cs offset]
     (if (and child offset cs)
       (let [rect (:chic.ui/component-rect ctx)]
         (assoc ctx :chic.ui/component-rect (.offset cs (:x rect) (:y rect))))
       ctx)))

(defn measure-child
  ([child ctx cs]
   (assert-good-cs cs)
   (huip/-measure child (child-ctx ctx child cs) cs))
  #_([child ctx cs offset]
     (huip/-measure child (child-ctx ctx child cs offset) cs)))

#_(defn measure-child-unbounded
    ([child ctx]
     (let [cs (IPoint. Integer/MAX_VALUE Integer/MAX_VALUE)]
       (huip/-measure child (child-ctx ctx child cs) cs))))

(defn rects-overlap? [r1 {:keys [x y right bottom]}]
  #_(or (.contains r1 x y)
      (.contains r1 x bottom)
      (.contains r1 right y)
      (.contains r1 right bottom))
  (and (<= (:x r1) right)
       (<= x (:right r1))
       (<= (:y r1) bottom)
       (<= y (:bottom r1))))

(defn draw-child
  ([child ctx cs canvas]
   (assert-good-cs cs)
   (when (rects-overlap? cs (:chic.ui/window-content-bounds ctx))
     (huip/-draw child (child-ctx ctx child cs) cs canvas)))
  #_([child ctx cs canvas offset]
     (huip/-draw child (child-ctx ctx child cs offset) cs canvas)))

(defmacro dyncomp [child]
  `(ui/dynamic _ctx# [_# (deref (var ~(first child)))]
               ~child))

(deftype+ Clickable [on-event child ^:mut child-rect ^:mut hovered? ^:mut pressed?]
  IComponent
  (-measure [_ ctx cs]
            (measure-child child ctx cs))

  (-draw [_ ctx cs canvas]
         (set! child-rect cs)
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
                 ;; (clojure.pprint/pprint event)
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
                                   (on-event (assoc event :hui/hovered? hovered?)))
                                 false))]
               (when (not= pressed? pressed?')
                 (set! pressed? pressed?')
                 true)))
           (event-propagate event child child-rect)))

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
             (on-event event))
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
