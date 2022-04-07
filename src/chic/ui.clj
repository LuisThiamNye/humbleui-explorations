(ns chic.ui
  (:require
   [chic.util :as util]
   [clojure.pprint :as pp]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.profile :as profile]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Canvas Font Paint]
   [io.github.humbleui.skija.shaper ShapingOptions Shaper]
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

(def ^:private ^Shaper shaper (Shaper/makeShapeDontWrapOrReorder))

(alter-var-root
 #'ui/label
 (fn [_]
   (fn label [^String text ^Font font ^Paint paint & features]
     {:pre [(some? font) (some? paint)]}
     (let [opts (reduce #(.withFeatures ^ShapingOptions %1 ^String %2) ShapingOptions/DEFAULT features)
           line (.shapeLine shaper text font ^ShapingOptions opts)]
       (ui/->Label text font paint line (.getMetrics ^Font font))))))

(alter-var-root
 #'hui/memoize-last
 (fn [_]
   (fn memoize-last [ctor]
     (let [*atom (volatile! nil)]
       (fn [& args']
         (or
          (when-some [[args value] @*atom]
            (if (some false? (map #(if (number? %1) (= %1 %2) (identical? %1 %2)) args args'))
              (when (instance? AutoCloseable value)
                (.close ^AutoCloseable value))
              value))
          (let [value' (apply ctor args')]
            (vreset! *atom [args' value'])
            value')))))))

#_(alter-var-root
   #'ui/dynamic
   (fn [_]
     (fn dynamic [&form &env ctx-sym bindings & body]
       (let [syms (ui/bindings->syms bindings)]
         `(let [inputs-fn# (core/memoize-last (fn [~@syms] ~@body))]
            (contextual
             (fn [~ctx-sym]
               (let [~@bindings]
                 (inputs-fn# ~@syms)))))))))

(defn -memoize-last-1arg [ctor]
  (let [*cache (volatile! nil)]
    (fn [args']
      (or
       (when-some [cache ^"[Ljava.lang.Object;" @*cache]
         (let [args (aget cache 0)
               value (aget cache 1)]
           (if (loop [i (unchecked-dec-int (count args'))]
                 (if (< i 0)
                   false
                   (let [a1 (nth args i)
                         a2 (nth args' i)]
                     (if (if (or (instance? Double a1) (instance? Float a1))
                           (clojure.lang.Util/equals a1 a2)
                           (clojure.lang.Util/identical a1 a2))
                       (recur (unchecked-dec-int i))
                       true))))
             (when (instance? AutoCloseable value)
               (.close ^AutoCloseable value))
             value)))
       (let [value' (ctor args')]
         (vreset! *cache (doto ^"[Ljava.lang.Object;" (make-array Object 2)
                           (aset 0 args')
                           (aset 1 value')))
         value')))))

(defmacro dynamic [ctx-sym bindings & body]
  (let [syms (ui/bindings->syms bindings)]
    `(let [inputs-fn# (-memoize-last-1arg (fn [[~@syms]] ~@body))]
       (ui/contextual
        (fn [~ctx-sym]
          (let [~@bindings]
            (inputs-fn# [~@syms])))))))

(defn assert-good-cs [cs]
  (when (or (instance? Point cs) (instance? IPoint cs)
            ;; (neg? (:right cs))
            ;; (neg? (:x cs))
            ;; (neg? (:bottom cs))
            ;; (neg? (:y cs))
            (neg? (:width cs)) (neg? (:height cs)))
    (throw (doto (ex-info "bad cs" {:cs cs}) pp/pprint)))
  cs)

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

(defn intersect-rect [^IRect rect subrect]
  (or (.intersect rect subrect) (rect-with-wh rect 0 0)))

(defmacro -component-typed-rect-contains? [cls rect pos]
  (let [rect (vary-meta rect assoc :tag cls)]
    `(let [{x# :x y# :y} ~pos
           rect# ~rect]
       (and (<= (.getLeft rect#) x#)
            (<= (.getTop rect#) y#)
            (< x# (.getRight rect#))
            (< y# (.getBottom rect#))))))

(defn component-irect-contains? [rect point]
  (-component-typed-rect-contains? IRect rect point))

(defn point-in-component? [{rect :chic.ui/component-rect} pos]
  {:pre [(some? pos)]}
  (if (instance? IRect rect)
    (-component-typed-rect-contains? IRect rect pos)
    (-component-typed-rect-contains? Rect rect pos)))

(defn point-visible? [{:keys [::point-visible?]} point]
  (if point-visible?
    (point-visible? point)
    true))

(defn push-point-clip [ctx point->visible?]
  (assoc ctx ::point-visible?
         (fn [point] (and (point-visible? ctx point)
                          (point->visible? point)))))

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
        (huip/-event child
                     (when-let [rect (:chic.ui/component-rect event)]
                       (assoc event :chic.ui/component-rect
                              (IRect/makeXYWH (+ (:x rect) (:x offset))
                                              (+ (:y rect) (:y offset))
                                              (:width child-rect) (:height child-rect))))))))))

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
                          (and (point-in-component? ctx pos)
                               (point-visible? ctx pos))))
         (let [ctx' (cond-> ctx
                      hovered? (assoc :hui/hovered? true)
                      (and pressed? hovered?) (assoc :hui/active? true))]
           (draw-child child ctx' cs canvas)))

  (-event [_ event]
          (hui/eager-or
           (when (or (= :hui/mouse-button (:hui/event event))
                     (= :hui/mouse-move (:hui/event event)))
             (let [hovered?' (and (point-in-component? event (:chic.ui/mouse-win-pos event))
                                  (point-visible? event (:chic.ui/mouse-win-pos event)))]
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

(deftype+ DrawHook [on-draw after-draw child]
  IComponent
  (-measure [_ ctx cs] (measure-child child ctx cs))
  (-draw [_ ctx cs ^Canvas canvas]
         (on-draw ctx cs canvas)
         (draw-child child ctx cs canvas)
         (after-draw ctx cs canvas))
  (-event [_ event] (huip/-event child event))
  AutoCloseable
  (close [_] (ui/child-close child)))

(defn on-draw
  ([on-draw child]
   (->DrawHook on-draw (fn [_ _ _]) child))
  ([on-draw after-draw child]
   (->DrawHook on-draw after-draw child)))

(deftype+ ShadowRect [dx dy blur spread colour child]
  IComponent
  (-measure [_ ctx cs] (measure-child child ctx cs))
  (-draw [_ ctx {:keys [width height] :as cs} ^Canvas canvas]
         (.drawRectShadow canvas (Rect/makeXYWH 0 0 width height)
                          dx dy blur spread colour)
         (draw-child child ctx cs canvas))
  (-event [_ event] (huip/-event child event))
  AutoCloseable
  (close [_] (ui/child-close child)))

(defn shadow-rect [dx dy blur spread colour child]
  (dyncomp
   (->ShadowRect (float dx) (float dy) (float blur) (float spread) (unchecked-int colour) child)))

(deftype+ WithBounds [key child ^:mut bounds]
  IComponent
  (-measure [_ ctx cs]
            (huip/-measure
             child (assoc ctx key (or bounds
                                      (IPoint.
                                       (-> (:width cs) (/ (:scale ctx)))
                                       (-> (:height cs) (/ (:scale ctx)))))) cs))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [width (-> (:width cs) (/ (:scale ctx)))
               height (-> (:height cs) (/ (:scale ctx)))
               bounds' (IPoint. width height)]
           (when-not (= bounds bounds')
             (set! bounds bounds'))
           (huip/-draw child (assoc ctx key bounds) cs canvas)))

  (-event [_ event] (huip/-event child event))

  AutoCloseable
  (close [_] (ui/child-close child)))

(defn with-bounds [key child]
  (->WithBounds key child nil))

(deftype+ EffectHandler [handler child]
  IComponent
  (-measure [_ ctx cs]
            (huip/-measure child ctx cs))

  (-draw [_ ctx cs ^Canvas canvas]
         (huip/-draw child ctx cs canvas))

  (-event [_ event]
          (huip/-event child
                       (assoc event ::effect-handler
                              (if-let [f (::effect-handler event)]
                                (comp f handler)
                                handler))))

  AutoCloseable
  (close [_] (ui/child-close child)))

(defn on [effectkey handler child]
  (->EffectHandler (fn [effects]
                     (reduce (fn [result effect]
                               (if (= effectkey (nth effect 0))
                                 (into result (handler (nth effect 1)))
                                 (conj result effect)))
                             []
                             effects)) child))

(defn emit [event effects]
  (when-let [f (::effect-handler event)]
    (f effects)))

(deftype+ UpdatingContext [xf child]
  IComponent
  (-measure [_ ctx cs] (measure-child child (xf ctx) cs))

  (-draw [_ ctx cs ^Canvas canvas]
         (draw-child child (xf ctx) cs canvas))

  (-event [_ event] (huip/-event child (xf event)))

  AutoCloseable (close [_] (ui/child-close child)))

(defn updating-ctx [f child]
  (dyncomp (->UpdatingContext f child)))

#_(deftype+ PathsSubscribe [child-ctor ^:mut child ^:mut outdated?]
    IComponent
    (-measure [_ ctx cs]
              (let [child' (child-ctor ctx)]
                (when-not (identical? child child')
                  (ui/child-close child)
                  (set! child child')))
              (huip/-measure child ctx cs))

    (-draw [_ ctx cs canvas]
           (let [child' (child-ctor ctx)]
             (when-not (identical? child child')
               (ui/child-close child)
               (set! child child')))
           (huip/-draw child ctx cs canvas))

    (-event [_ event] (huip/-event child event))

    AutoCloseable (close [_] (ui/child-close child)))

#_(defn subscribe-paths [child]
    (dyncomp (->PathsSubscribe child)))

(deftype+ WithBounds [k child ^:mut bounds]
  IComponent
  (-measure [_ ctx cs]
            (huip/-measure child
                           (assoc ctx k (or bounds
                                            (IPoint. (-> (:width cs) (/ (:scale ctx)))
                                                     (-> (:height cs) (/ (:scale ctx)))))) cs))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [width (-> (:width cs) (/ (:scale ctx)))
               height (-> (:height cs) (/ (:scale ctx)))
               bounds' (IPoint. width height)]
           (when-not (= bounds bounds')
             (set! bounds bounds'))
           (huip/-draw child (assoc ctx k bounds) cs canvas)))

  (-event [_ event] (huip/-event child event))

  AutoCloseable
  (close [_] (ui/child-close child)))

(defn with-bounds [key child]
  (->WithBounds key child nil))

(deftype+ Lifecycle [on-mount on-close child ^:mut initialised?]
  IComponent
  (-measure [_ ctx cs] (huip/-measure child ctx cs))

  (-draw [self ctx cs ^Canvas canvas]
         (when-not initialised?
           (when on-mount (on-mount self ctx))
           (set! initialised? true))
         (huip/-draw child ctx cs canvas))

  (-event [_ event] (huip/-event child event))

  AutoCloseable
  (close [self] (when on-close (on-close self)) (ui/child-close child)))

(defn lifecycle [on-mount on-close child]
  (->Lifecycle on-mount on-close child false))

(deftype+ ProfileDraw [title child]
  IComponent
  (-measure [_ ctx cs]
            (profile/measure (str title " : measure")
                             (huip/-measure child ctx cs)))

  (-draw [self ctx cs ^Canvas canvas]
         (profile/measure title (huip/-draw child ctx cs canvas)))

  (-event [_ event] (huip/-event child event))

  AutoCloseable
  (close [self] (ui/child-close child)))

(defn profile [nam child]
  (->ProfileDraw nam child))
