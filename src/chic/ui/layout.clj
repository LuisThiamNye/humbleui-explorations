(ns chic.ui.layout
  (:require
   [clojure.math :as math]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.ui :as ui]
   [chic.ui :as cui :refer [draw-child measure-child child-ctx]])
  (:import
   [java.lang AutoCloseable]
   [io.github.humbleui.types IPoint IRect Point Rect RRect]
   [io.github.humbleui.skija Canvas Font FontMetrics Paint TextLine]
   [io.github.humbleui.skija.shaper Shaper ShapingOptions]))

(deftype+ HAlign [child-coeff coeff child ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
            (measure-child child ctx cs))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [layer (.save canvas)
               child-size (huip/-measure child ctx cs)
               left (- (* (:width cs) coeff) (* (:width child-size) child-coeff))]
           (set! child-rect (IRect/makeXYWH left 0 (:width child-size) (:height cs)))
           (try
             (.translate canvas left 0)
             (draw-child child ctx child-rect canvas {:x left :y 0})
             (finally
               (.restoreToCount canvas layer)))))

  (-event [_ event]
          (ui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
         (ui/child-close child)))

(defn halign
  ([coeff child] (halign coeff coeff child))
  ([child-coeff coeff child] (->HAlign child-coeff coeff child nil)))

(deftype+ VAlign [child-coeff coeff child ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
            (measure-child child ctx cs))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [layer (.save canvas)
               child-size (huip/-measure child ctx cs)
               top (- (* (:height cs) coeff) (* (:height child-size) child-coeff))]
           (set! child-rect (IRect/makeXYWH 0 top (:width cs) (:height child-size)))
           (try
             (.translate canvas 0 top)
             (draw-child child ctx child-rect canvas {:x 0 :y top})
             (finally
               (.restoreToCount canvas layer)))))

  (-event [_ event]
          (ui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
         (ui/child-close child)))

(defn valign
  ([coeff child] (valign coeff coeff child))
  ([child-coeff coeff child] (->VAlign child-coeff coeff child nil)))

(deftype+ Width [value child ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
            (let [width' (ui/dimension value cs ctx)
                  child-size (measure-child child ctx (assoc cs :width width'))]
              (assoc child-size :width width')))

  (-draw [_ ctx cs ^Canvas canvas]
         (set! child-rect (IRect/makeXYWH 0 0 (:width cs) (:height cs)))
         (draw-child child ctx cs canvas))

  (-event [_ event]
          (ui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
         (ui/child-close child)))

(defn width [value child]
  (->Width value child nil))

(deftype+ Height [value child ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
            (let [height' (ui/dimension value cs ctx)
                  child-size (measure-child child ctx (assoc cs :height height'))]
              (assoc child-size :height height')))

  (-draw [_ ctx cs ^Canvas canvas]
         (set! child-rect (IRect/makeXYWH 0 0 (:width cs) (:height cs)))
         (draw-child child ctx cs canvas))

  (-event [_ event]
          (ui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
         (ui/child-close child)))

(defn height [value child]
  (->Height value child nil))

(deftype+ Column [children ^:mut child-rects]
  IComponent
  (-measure [_ ctx cs]
            (reduce
             (fn [{:keys [width height]} child]
               (let [child-size (measure-child child ctx cs {:x 0 :y height})]
                 (IPoint. (max width (:width child-size)) (+ height (:height child-size)))))
             (IPoint. 0 0)
             (keep #(nth % 2) children)))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [known (for [[mode _ child] children]
                       (when (= :hug mode)
                         (huip/-measure child ctx cs)))
               space (- (:height cs) (transduce (keep :height) + 0 known))
               stretch (transduce (keep (fn [[mode value _]] (when (= :stretch mode) value))) + 0 children)
               layer (.save canvas)]
           (try
             (loop [height 0
                    rects []
                    known known
                    children children]
               (if-some [[mode value child] (first children)]
                 (let [child-size (case mode
                                    :hug (first known)
                                    :stretch (IPoint. (:width cs) (-> space (/ stretch) (* value) (math/round))))]
                   (when child
                     (draw-child child ctx (assoc child-size :width (:width cs)) canvas {:x 0 :y height}))
                   (.translate canvas 0 (:height child-size))
                   (recur
                    (+ height (long (:height child-size)))
                    (conj rects (IRect/makeXYWH 0 height (:width cs) (:height child-size)))
                    (next known)
                    (next children)))
                 (set! child-rects rects)))
             (.restoreToCount canvas layer))))

  (-event [_ event]
          (reduce
           (fn [acc [[_ _ child] rect]]
             (hui/eager-or acc (ui/event-propagate event child rect)))
           false
           (hui/zip children child-rects)))

  AutoCloseable
  (close [_]
         (doseq [[_ _ child] children]
           (ui/child-close child))))

(defn flatten-container [children]
  (into []
        (mapcat
         #(cond
            (nil? %) []
            (vector? %) [%]
            (sequential? %) (flatten-container %)
            :else [[:hug nil %]]))
        children))

(defn column [& children]
  (->Column (flatten-container children) nil))

(deftype+ Row [children ^:mut child-rects]
  IComponent
  (-measure [_ ctx cs]
            (reduce
             (fn [{:keys [width height]} child]
               (let [child-size (measure-child child ctx cs {:x width :y 0})]
                 (IPoint. (+ width (:width child-size)) (max height (:height child-size)))))
             (IPoint. 0 0)
             (keep #(nth % 2) children)))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [known (for [[mode _ child] children]
                       (when (= :hug mode)
                         (measure-child child ctx cs)))
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
                                  :stretch (IPoint. (-> space (/ stretch) (* value) (math/round)) (:height cs)))]
                 (when child
                   (draw-child child ctx (assoc child-size :height (:height cs)) canvas
                               {:x width :y 0}))
                 (.translate canvas (:width child-size) 0)
                 (recur
                  (+ width (long (:width child-size)))
                  (conj rects (IRect/makeXYWH width 0 (:width child-size) (:height cs)))
                  (next known)
                  (next children)))
               (set! child-rects rects)))
           (.restoreToCount canvas layer)))

  (-event [_ event]
          (reduce
           (fn [acc [[_ _ child] rect]]
             (hui/eager-or acc (ui/event-propagate event child rect) false))
           false
           (hui/zip children child-rects)))

  AutoCloseable
  (close [_]
         (doseq [[_ _ child] children]
           (ui/child-close child))))

(defn row [& children]
  (cui/dyncomp
   (->Row (flatten-container children) nil)))

(deftype+ Padding [left top right bottom child ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
            (let [left' (ui/dimension left cs ctx)
                  right' (ui/dimension right cs ctx)
                  top' (ui/dimension top cs ctx)
                  bottom' (ui/dimension bottom cs ctx)
                  child-cs (IPoint. (- (:width cs) left' right') (- (:height cs) top' bottom'))
                  child-size (measure-child child ctx child-cs {:x left' :y top'})]
              (IPoint.
               (+ (:width child-size) left' right')
               (+ (:height child-size) top' bottom'))))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [{:keys [scale]} ctx
               left' (ui/dimension left cs ctx)
               right' (ui/dimension right cs ctx)
               top' (ui/dimension top cs ctx)
               bottom' (ui/dimension bottom cs ctx)
               layer (.save canvas)
               width' (- (:width cs) left' right')
               height' (- (:height cs) top' bottom')]
           (set! child-rect (IRect/makeXYWH left' top' width' height'))
           (try
             (.translate canvas left' top')
             (draw-child child ctx (IPoint. width' height') canvas
                         {:x left' :y top'})
             (finally
               (.restoreToCount canvas layer)))))

  (-event [_ event]
          (ui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
         (ui/child-close child)))

(defn padding
  ([p child] (->Padding p p p p child nil))
  ([w h child] (->Padding w h w h child nil))
  ([l t r b child] (->Padding l t r b child nil)))

(deftype+ VScroll [child ^:mut offset ^:mut size ^:mut child-size]
  IComponent
  (-measure [_ ctx cs]
            (let [child-cs (assoc cs :height Integer/MAX_VALUE)]
              (set! child-size (measure-child child ctx child-cs))
              (set! offset (hui/clamp offset (- (:height cs) (:height child-size)) 0))
              (IPoint. (:width child-size) (:height cs))))

  (-draw [_ ctx cs ^Canvas canvas]
         (set! size cs)
         (let [layer (.save canvas)
               child-cs (assoc cs :height Integer/MAX_VALUE)]
           (try
             (.clipRect canvas (Rect/makeXYWH 0 0 (:width cs) (:height cs)))
             (.translate canvas 0 offset)
             (draw-child child ctx child-cs canvas {:x 0 :y offset})
             (finally
               (.restoreToCount canvas layer)))))

  (-event [_ event]
          (let [changed? (and (some->> (:chic.ui/mouse-win-pos event)
                                       (cui/point-in-component? event))
                           (not= 0 (:hui.event.mouse-scroll/dy event 0)))
                _ (when changed?
                    (set! offset (-> offset
                                     (+ (:hui.event.mouse-scroll/dy event))
                                     (hui/clamp (- (:height size) (:height child-size)) 0))))
                child-rect (IRect/makeLTRB 0 0 (min (:width child-size) (:width size)) (min (:height child-size) (:height size)))]
            (hui/eager-or changed?
                          (ui/event-propagate event child child-rect (IPoint. 0 offset)))))

  AutoCloseable
  (close [_]
         (ui/child-close child)))

(defn vscroll [child]
  (->VScroll child 0 nil nil))

(defn vscrollbar [child]
  (when-not (instance? VScroll child)#_(= "chic.ui.layout.VScroll" (.getName (class child)))
    (throw (ex-info (str "Expected VScroll, got: " (type child)) {:child child})))
  (ui/dynamic _ [_ (resolve '->VScroll)]
              (ui/->VScrollbar child
                               (doto (Paint.) (.setColor (unchecked-int 0x10000000)))
                               (doto (Paint.) (.setColor (unchecked-int 0x60000000)))
                               nil)))

(deftype+ SizeDependent [childf ^:mut child]
  IComponent
  (-measure [_ ctx cs]
            (set! child (childf cs))
            (measure-child child ctx cs))
  (-draw [_ ctx cs ^Canvas canvas]
         (set! child (childf cs))
         (draw-child child ctx cs canvas))
  (-event [_ event] (huip/-event child event))
  AutoCloseable
  (close [_] (ui/child-close child)))

(defn size-dependent [childf]
  (cui/dyncomp
   (->SizeDependent childf nil)))

(deftype+ Scrollable [on-scroll child]
  IComponent
  (-measure [_ ctx cs]
            (measure-child child ctx cs))

  (-draw [_ ctx cs canvas]
         (draw-child child ctx cs canvas))

  (-event [_ event]
          (hui/eager-or
           (when (= :hui/mouse-scroll (:hui/event event))
             (on-scroll event)
             true)
           (huip/-event child event)))

  AutoCloseable
  (close [_] (ui/child-close child)))

(defn scrollable [on-scroll child]
  (cui/dyncomp
   (->Scrollable on-scroll child)))

(deftype+ OverflowX [child target-offset ^:mut offset ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
            (let [child-cs (assoc cs :width Integer/MAX_VALUE)]
              (set! child-rect (measure-child child ctx child-cs {:x target-offset :y 0}))
              (set! offset (hui/clamp target-offset (- (:width cs) (:width child-rect)) 0))
              (IPoint. (:width cs) (:height child-rect))))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [child-size (measure-child child ctx (assoc cs :width Integer/MAX_VALUE))]
           (set! child-rect (IRect/makeXYWH offset 0 (:width child-size) (:height child-size))))
         (set! offset (hui/clamp target-offset (- (:width cs) (:width child-rect)) 0))
         (let [layer (.save canvas)
               child-cs child-rect]
           (try
             (.clipRect canvas (Rect/makeXYWH 0 0 (:width cs) (:height cs)))
             (.translate canvas offset 0)
             (draw-child child ctx child-cs canvas{:x offset :y 0})
             (finally
               (.restoreToCount canvas layer)))))

  (-event [_ event] (ui/event-propagate event child child-rect {:x offset :y 0}))

  AutoCloseable
  (close [_]
         (ui/child-close child)))

(defn overflow-x [x child]
  (cui/dyncomp
   (->OverflowX child x 0 nil)))

(deftype+ Stack [children]
  IComponent
  (-measure [_ ctx cs]
            (reduce
             (fn [{:keys [width height]} child]
               (let [child-size (measure-child child ctx cs)]
                 (IPoint. (max width (:width child-size))
                          (max height (:height child-size)))))
             (IPoint. 0 0)
             children))

  (-draw [_ ctx cs ^Canvas canvas]
         (doseq [child children]
           (draw-child child ctx cs canvas)))

  (-event [_ event]
          (reduce
           (fn [acc child]
             (hui/eager-or acc (huip/-event child event)))
           false
           children))

  AutoCloseable
  (close [_]
         (run! ui/child-close children)))

(defn stack [& children]
  (cui/dyncomp
   (->Stack (eduction (remove nil?) children))))
