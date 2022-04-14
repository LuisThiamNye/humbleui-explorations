(ns chic.ui.layout
  (:require
   [taoensso.encore :as enc]
   [io.github.humbleui.profile :as profile]
   [proteus :refer [let-mutable]]
   [chic.util :as util]
   [better-cond.core :refer [cond] :rename {cond cond+}]
   [clj-commons.primitive-math :as prim]
   [potemkin :refer [doit]]
   [chic.ui :as cui :refer [draw-child measure-child]]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui]
   [chic.humbleui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Canvas Paint]
   [io.github.humbleui.types IPoint IRect Rect Point RRect]
   [java.lang AutoCloseable]))

(deftype+ HAlign [child-coeff coeff child ^:mut child-rect ^:mut child-size]
  IComponent
  (-measure [_ ctx cs]
    (set! child-size (measure-child child ctx cs)))

  (-draw [_ ctx cs ^Canvas canvas]
    (let [layer (.save canvas)
          child-size' (or child-size (measure-child child ctx cs))
          left (int (- (* (:width cs) coeff)
                       (* (:width child-size') child-coeff)))]
      (set! child-size nil)
      (set! child-rect (cui/offset-lw cs left (:width child-size')))
      (try
        (.translate canvas left 0)
        (draw-child child ctx child-rect canvas)
        (finally
          (.restoreToCount canvas layer)))))

  (-event [_ event]
    (cui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
    (ui/child-close child)))

(defn halign
  ([coeff child] (halign coeff coeff child))
  ([child-coeff coeff child] (->HAlign child-coeff coeff child nil nil)))

(deftype+ VAlign [child-coeff coeff child ^:mut child-rect ^:mut child-size]
  IComponent
  (-measure [_ ctx cs]
    (set! child-size (measure-child child ctx cs)))

  (-draw [_ ctx cs ^Canvas canvas]
    (let [layer (.save canvas)
          child-size' (or child-size (measure-child child ctx cs))
          top (int (- (* (:height cs) coeff) (* (:height child-size') child-coeff)))]
      (set! child-size nil)
      (set! child-rect (cui/offset-th cs top (:height child-size')))
      (try
        (.translate canvas 0 top)
        (draw-child child ctx child-rect canvas)
        (finally
          (.restoreToCount canvas layer)))))

  (-event [_ event]
    (cui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
    (ui/child-close child)))

(defn valign
  ([coeff child] (valign coeff coeff child))
  ([child-coeff coeff child] (->VAlign child-coeff coeff child nil nil)))

(deftype+ Width [value child ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
    (let [width' (ui/dimension value cs ctx)
          child-size (measure-child child ctx (assoc cs :width width'))]
      (assoc child-size :width width')))

  (-draw [_ ctx cs ^Canvas canvas]
    (set! child-rect cs)
    (draw-child child ctx cs canvas))

  (-event [_ event]
    (cui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
    (ui/child-close child)))

(defn width [value child]
  (cui/dyncomp (->Width value child nil)))

(deftype+ Height [value child ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
    (let [height' (ui/dimension value cs ctx)
          child-size (measure-child child ctx (assoc cs :height height'))]
      (assoc child-size :height height')))

  (-draw [_ ctx cs ^Canvas canvas]
    (set! child-rect cs)
    (draw-child child ctx cs canvas))

  (-event [_ event]
    (cui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
    (ui/child-close child)))

(defn height [value child]
  (cui/dyncomp (->Height value child nil)))

(deftype+ Column [^java.util.ArrayList children ^java.util.ArrayList child-sizes ^java.util.ArrayList child-rects]
  IComponent
  (-measure [_ ctx rect]
    (.clear child-sizes)
    (let [max-height ^int (:height rect)
          nchildren (.size children)]
      (let-mutable
       [width (unchecked-int 0)
        height (unchecked-int 0)]
        (loop [i (unchecked-int 0)]
          (when (and (prim/< i nchildren)
                     #_(< height max-height))
            (when-some [child (nth (.get children i) 2)]
             (let [child-size (measure-child
                               child ctx (cui/offset-lt
                                          rect 0 (min height max-height)))]
               (.add child-sizes child-size)
               (set! width (max width (:width child-size)))
               (set! height (+ height (:height child-size)))
               (recur (unchecked-inc-int i))))))
        (cui/rect-with-wh rect width height))))

  (-draw [_ ctx cs ^Canvas canvas]
    (let [*known-height (proteus.Containers$L. 0)
          *stretch (proteus.Containers$L. 0)
          known (java.util.ArrayList. (.size children))
          get-child-size (if (< 0 (.size child-sizes))
                           #(.get child-sizes %2)
                           (fn [child _] (measure-child child ctx cs)))
          _ (dotimes [i (.size children)]
              (let [child (.get children i)
                    [mode ^long value child] child]
                (if (= :hug mode)
                  (let [size (get-child-size child i)]
                   (.set *known-height (prim/+ (.-x *known-height) ^int (:height size)))
                   (.add known size))
                 (do (.add known nil)
                     (.set *stretch (unchecked-add (.-x *stretch) value))))))
          space (max 0 (prim/- ^int (:height cs) (.-x *known-height)))
          stretch (.-x *stretch)
          max-y (:height cs)
          layer (.save canvas)
          nchildren (.size children)]
      (.clear child-sizes)
      (.clear child-rects)
      (try
        (loop [y (unchecked-int 0)
               i (unchecked-int 0)]
          (if (prim/< i nchildren)
            (let [[mode ^long value child] (.get children i)
                  child-height (:height (case mode
                                          :hug (.get known i)
                                          :stretch (IPoint. 0 (-> space (/ stretch) (* value) (math/round)))))
                  rect (cui/offset-ltrb cs 0 (min y max-y) 0 (max 0 (- max-y child-height y)))]
              (when child
                (draw-child child ctx rect canvas))
              (.translate canvas 0 child-height)
              (.add child-rects rect)
              (recur
               (prim/+ y ^int child-height)
               (prim/inc i)))))
        (finally (.restoreToCount canvas layer)))))

  (-event [_ event]
    (reduce
     (fn [acc [[_ _ child] rect]]
       (hui/eager-or acc (cui/event-propagate event child rect)))
     false
     (hui/zip children child-rects)))

  AutoCloseable
  (close [_]
    (doit [[_ _ child] children]
      (ui/child-close child))))

(defn ^java.util.ArrayList flatten-container [children]
  (let [lst (java.util.ArrayList.)
        fc (fn fc [children]
             (doit [child children]
               (cond+
                (vector? child) (.add lst child)
                (sequential? child) (fc child)
                (nil? child) nil
                (.add lst [:hug nil child]))))]
    (fc children)
    lst))

(defn column [& children]
  (let [childs (flatten-container children)
        nchildren (.size childs)]
    (->Column childs (java.util.ArrayList. nchildren)
              (java.util.ArrayList. nchildren))))

(deftype+ Row [^java.util.ArrayList children ^java.util.ArrayList child-sizes ^java.util.ArrayList child-rects]
  IComponent
  (-measure
    [_ ctx cs]
    (.clear child-sizes)
    (let-mutable [width 0
                  height 0]
      (doit [child-c children]
        (when-some [child (nth child-c 2)]
          (let [child-size (measure-child
                            child ctx (cui/offset-lt cs (min width (- (:right cs) (:x cs))) 0))]
            (.add child-sizes child-size)
            (set! width (+ width (:width child-size)))
            (set! height (max height (:height child-size))))))
      (cui/rect-with-wh cs width height)))

  (-draw [_ ctx cs ^Canvas canvas]
    (let [*known-width (proteus.Containers$L. 0)
          *stretch (proteus.Containers$L. 0)
          known (java.util.ArrayList. (.size children))
          get-child-size (if (< 0 (.size child-sizes))
                           #(.get child-sizes %2)
                           (fn [child _] (measure-child child ctx cs)))
          _ (dotimes [i (.size children)]
              (let [[mode ^long value child] (.get children i)]
                (if (= :hug mode)
                  (let [size (get-child-size child i)]
                    (.set *known-width (prim/+ (.-x *known-width) ^int (:width size)))
                    (.add known size))
                  (do (.add known nil)
                      (.set *stretch (prim/+ (.-x *stretch) value))))))
          space (max 0 (prim/- ^int (:width cs) (.-x *known-width)))
          stretch (.-x *stretch)
          layer (.save canvas)
          max-dx (:width cs)
          nchildren (.size children)]
      (.clear child-sizes)
      (.clear child-rects)
      (loop [dx (unchecked-int 0)
             i (unchecked-int 0)]
        (if (prim/< i nchildren)
          (let [[mode ^long value child] (.get children i)
                child-size (case mode
                             :hug (.get known i)
                             :stretch (IPoint. (-> space (* value) (/ stretch) (math/round)) 0))
                child-width (:width child-size)
                rect (cui/offset-ltrb cs (min dx max-dx) 0 (max 0 (- max-dx child-width dx)) 0)]
            (when child
              (draw-child child ctx rect canvas))
            (.translate canvas child-width 0)
            (.add child-rects rect)
            (recur
             (prim/+ dx ^int child-width)
             (prim/inc i)))))
      (.restoreToCount canvas layer)))

  (-event [_ event]
    (reduce
     (fn [acc [[_ _ child] rect]]
       (hui/eager-or acc (cui/event-propagate event child rect) false))
     false
     (hui/zip children child-rects)))

  AutoCloseable
  (close [_]
    (doit [[_ _ child] children]
      (ui/child-close child))))

(defn row [& children]
  (let [childs (flatten-container children)
        nchildren (.size childs)]
    (->Row childs (java.util.ArrayList. nchildren)
           (java.util.ArrayList. nchildren))))

(deftype+ Padding [left top right bottom child ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
    (let [left' (min (ui/dimension left cs ctx) (Math/floor (/ (:width cs) 2)))
          right' (min (ui/dimension right cs ctx) (Math/ceil (/ (:width cs) 2)))
          top' (min (ui/dimension top cs ctx) (Math/floor (/ (:height cs) 2)))
          bottom' (min (ui/dimension bottom cs ctx) (Math/ceil (/ (:height cs) 2)))
          child-cs (try (cui/offset-ltrb cs left' top' right' bottom')
                        (catch Exception e
                          (prn cs left' top' right' bottom')
                          (throw e)))
          child-size (measure-child child ctx child-cs)]
      (IPoint.
       (+ (:width child-size) left' right')
       (+ (:height child-size) top' bottom'))))

  (-draw [_ ctx cs ^Canvas canvas]
    (let [{:keys [scale]} ctx
          left' (ui/dimension left cs ctx)
          right' (ui/dimension right cs ctx)
          top' (ui/dimension top cs ctx)
          bottom' (ui/dimension bottom cs ctx)
          layer (.save canvas)]
           ;; (prn left top right bottom cs left' right' top' bottom')
      (set! child-rect (cui/offset-ltrb
                        cs left' top'
                        (min right' (unchecked-subtract (:width cs) left'))
                        (min bottom' (unchecked-subtract (:height cs) top'))))
      (try
        (.translate canvas left' top')
        (draw-child child ctx child-rect canvas)
        (finally
          (.restoreToCount canvas layer)))))

  (-event [_ event]
    (cui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
    (ui/child-close child)))

(defn padding
  ([p child] (->Padding p p p p child nil))
  ([w h child] (->Padding w h w h child nil))
  ([l t r b child] (->Padding l t r b child nil)))

(deftype+ VScroll [child ^:mut offset ^:mut size ^:mut child-rect ^:mut child-size]
  IComponent
  (-measure [_ ctx cs]
    (set! child-size (measure-child child ctx (cui/unbounded-bottom cs))))

  (-draw [_ ctx cs ^Canvas canvas]
    (set! size cs)
    (let [child-height (:height (or child-size (measure-child child ctx (cui/unbounded-bottom cs))))
          layer (.save canvas)]
      (set! child-size nil)
      (set! offset (hui/clamp offset (- (:height cs) child-height) 0))
      (set! child-rect (cui/rect-translate
                        (cui/rect-with-wh cs (:width cs) (max (:height cs) child-height))
                        0 offset))
      (try
        (.clipRect canvas (Rect/makeXYWH 0 0 (:width cs) (:height cs)))
        (.translate canvas 0 offset)
        (draw-child child (cui/push-point-clip
                           ctx (fn [point] (cui/component-irect-contains?
                                            (cui/intersect-rect size child-rect) point)))
                    child-rect canvas)
        (finally
          (.restoreToCount canvas layer)))))

  (-event [_ event]
    (let [changed? (and (some->> (:chic.ui/mouse-win-pos event)
                                 (cui/point-in-component? event))
                        (not= 0 (:hui.event.mouse-scroll/dy event 0)))
          _ (when changed?
              (set! offset (-> offset
                               (+ (:hui.event.mouse-scroll/dy event))
                               (hui/clamp (- (:height size) (:height child-rect)) 0))))]
      (hui/eager-or
       changed? (cui/event-propagate
                 (cui/push-point-clip
                  event (fn [point] (cui/component-irect-contains? (cui/intersect-rect size child-rect) point)))
                 child child-rect))))

  AutoCloseable
  (close [_]
    (ui/child-close child)))

(defn vscroll [child]
  (->VScroll child 0 nil nil nil))

(comment
  ;; closures do not expose mutable references but rather the object at the time of closure creation
  (deftype+ X [^:mut x]
    IComponent (-event [_ _] (fn [] x)))
  (let [in (->X 5)
        f (huip/-event in nil)]
    (hui/-set! in :x 7)
    (f))
  #!
  )

(deftype+ HScroll [child ^:mut offset ^:mut size ^:mut child-rect ^:mut child-size]
  IComponent
  (-measure [_ ctx cs]
    (set! child-size (cui/rect-with-wh cs (measure-child child ctx (cui/unbounded-right cs)))))

  (-draw [_ ctx cs ^Canvas canvas]
    (set! size cs)
    (let [child-width (:width (or child-size (measure-child child ctx (cui/unbounded-right cs))))
          layer (.save canvas)]
      (set! child-size nil)
      (set! offset (hui/clamp offset (- (:width cs) child-width) 0))
      (set! child-rect (cui/rect-translate
                        (cui/rect-with-wh cs (max (:width cs) child-width) (:height cs))
                        offset 0))
      (try
        (.clipRect canvas (Rect/makeXYWH 0 0 (:width cs) (:height cs)))
        (.translate canvas offset 0)
        (draw-child child (cui/push-point-clip
                           ctx (fn [point] (cui/component-irect-contains?
                                            (cui/intersect-rect size child-rect) point)))
                    child-rect canvas)
        (finally
          (.restoreToCount canvas layer)))))

  (-event [_ event]
    (let [new-offset
          (and (some->> (:chic.ui/mouse-win-pos event)
                        (cui/point-in-component? event))
               (not= 0 (:hui.event.mouse-scroll/dx event 0))
               (-> offset
                   (+ (:hui.event.mouse-scroll/dx event))
                   (hui/clamp (- (:width size) (:width child-rect)) 0)))]
      (if (and new-offset (not (== new-offset offset)))
        (let [*children-scrolling? (volatile! false)
              request-scroll (fn request-scroll [evt dx dy]
                               (vreset! *children-scrolling? true))
              child-changed? (cui/event-propagate (assoc event ::request-scroll request-scroll)
                                                  child child-rect)]
          (hui/eager-or
           child-changed?
           (when (and (not @*children-scrolling?)
                      (if-let [rs (::request-scroll event)]
                        (rs event (:hui.event.mouse-scroll/dx event 0) 0)
                        true))
             (set! offset new-offset))))
        (cui/event-propagate (cui/push-point-clip
                              event (fn [point] (cui/component-irect-contains? (cui/intersect-rect size child-rect) point)))
                             child child-rect))))

  AutoCloseable
  (close [_]
    (ui/child-close child)))

(defn hscroll [child]
  (cui/dyncomp (->HScroll child 0 nil nil nil)))

(deftype+ VScrollbar [child ^Paint fill-track ^Paint fill-thumb ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
    (cui/measure-child child ctx cs))

  (-draw [_ ctx cs ^Canvas canvas]
    (set! child-rect cs)
    (draw-child child ctx cs canvas)
    (let [{:keys [scale]} ctx
          content-y (- (:offset child))
          content-h (:height (:child-rect child))
          scroll-y (- (:y child-rect) (:y cs))
          scroll-h (:height cs)
          scroll-r (- (:right child-rect) (:x cs))

          padding (* 4 scale)
          track-w (* 4 scale)
          track-x (- (:width cs) track-w padding)
          track-y (+ scroll-y padding)
          track-h (- scroll-h (* 2 padding))
          track (RRect/makeXYWH track-x track-y track-w track-h (* 2 scale))

          thumb-w (* 4 scale)
          min-thumb-h (* 16 scale)
          scroll-ratio (if (< scroll-h content-h)
                         (/ content-y (- content-h scroll-h))
                         0)
          thumb-h (if (< scroll-h content-h)
                    (max (* (/ scroll-h content-h) track-h) min-thumb-h)
                    track-h)
          thumb-y (+ track-y (* scroll-ratio (- track-h thumb-h)))
          thumb (RRect/makeXYWH track-x thumb-y thumb-w thumb-h (* 2 scale))]
      (.drawRRect canvas track fill-track)
      (.drawRRect canvas thumb fill-thumb)))

  (-event [_ event]
    (cui/event-propagate event child child-rect))

  AutoCloseable
  (close [_]
    ;; TODO causes crash
    ; (.close fill-track)
    ; (.close fill-thumb)
    (ui/child-close child)))

(defn vscrollbar [child]
  (when-not #_(instance? (Class/forName "chic.ui.layout.VScroll") child)
   (= "chic.ui.layout.VScroll" (.getName (class child)))
    (throw (ex-info (str "Expected VScroll, got: " (type child)) {:child child})))
  (cui/dyncomp
   (->VScrollbar child
                 (doto (Paint.) (.setColor (unchecked-int 0x10000000)))
                 (doto (Paint.) (.setColor (unchecked-int 0x60000000)))
                 nil)))

(deftype+ SizeDependent [childf ^:mut child]
  IComponent
  (-measure [_ ctx cs]
    (let [child' (childf cs)]
      (when-not (identical? child child')
        (ui/child-close child)
        (set! child child')))
    (measure-child child ctx cs))
  (-draw [_ ctx cs ^Canvas canvas]
    (let [child' (childf cs)]
      (when-not (identical? child child')
        (ui/child-close child)
        (set! child child')))
    (draw-child child ctx cs canvas))
  (-event [_ event] (huip/-event child event))
  AutoCloseable
  (close [_] (ui/child-close child)))

(defn size-dependent [childf]
  (cui/dyncomp
   (->SizeDependent childf nil)))
#_#_(deftype+ ChildSizeDependent [inner-child child-ctor ^:mut child]
      IComponent
      (-measure [_ ctx cs]
        (let [child' (child-ctor cs)]
          (when-not (identical? child child')
            (ui/child-close child)
            (set! child child')))
        (measure-child child ctx cs))
      (-draw [_ ctx cs ^Canvas canvas]
        (let [ichild-size (cui/measure-child inner-child ctx cs)
              child' (child-ctor cs)]
          (when-not (identical? child child')
            (ui/child-close child)
            (set! child child')))
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
    (let [child-cs (cui/unbounded-right cs)
          target-offset (ui/dimension target-offset cs ctx)]
      (set! child-rect (cui/rect-with-wh cs (measure-child child ctx (cui/rect-translate child-cs target-offset 0))))
      (set! offset (hui/clamp target-offset (- (:width cs) (:width child-rect)) 0))
      (IPoint. (:width cs) (:height child-rect))))

  (-draw [_ ctx cs ^Canvas canvas]
    (let [child-size (measure-child child ctx (cui/unbounded-right cs))]
      (set! offset (hui/clamp (ui/dimension target-offset cs ctx) (- (:width cs) (:width child-size)) 0))
      (set! child-rect (cui/rect-with-wh (cui/rect-translate cs offset 0)
                                         (:width child-size) (:height child-size))))
    (let [layer (.save canvas)
          ctx (cui/push-point-clip
               ctx (fn [point] (cui/component-irect-contains?
                                (cui/intersect-rect child-rect cs) point)))]
      (try
        (.clipRect canvas (Rect/makeXYWH 0 0 (:width cs) (:height cs)))
        (.translate canvas offset 0)
        (draw-child child ctx child-rect canvas)
        (finally
          (.restoreToCount canvas layer)))))

  (-event [_ event] (cui/event-propagate
                     (cui/push-point-clip
                      event (fn [point] (cui/component-irect-contains?
                                         (cui/intersect-rect child-rect (:chic.ui/component-rect event)) point)))
                     child child-rect))

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
    (doit [child children]
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

(defn flatten-stack-children [children]
  (eduction (mapcat (fn [child]
                      (if (sequential? child)
                        (flatten-stack-children child)
                        [child])))
            children))

(defn stack [& children]
  (cui/dyncomp
   (->Stack (persistent! (enc/into! (transient []) (remove nil?) (flatten-stack-children children))))))

(deftype+ Translate [dx dy child ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs] (cui/measure-child child ctx cs))

  (-draw [_ ctx cs ^Canvas canvas]
    (let [layer (.save canvas)
          dx (ui/dimension dx cs ctx)
          dy (ui/dimension dy cs ctx)]
      (set! child-rect (cui/rect-translate cs dx dy))
      (try (.translate canvas dx dy)
           (cui/draw-child child ctx child-rect canvas)
           (finally (.restoreToCount canvas layer)))))

  (-event [_ event] (cui/event-propagate event child child-rect))

  AutoCloseable
  (close [_] (ui/child-close child)))

(defn translate [dx dy child]
  (->Translate dx dy child nil))
