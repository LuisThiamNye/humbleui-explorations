(ns chic.ui.layout
  (:require
   [taoensso.encore :as enc]
   [chic.ui :as cui :refer [draw-child measure-child]]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui]
   [chic.humbleui  :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Canvas Paint]
   [io.github.humbleui.types IPoint IRect Rect Point RRect]
   [java.lang AutoCloseable]))

(deftype+ HAlign [child-coeff coeff child ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
            (measure-child child ctx cs))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [layer (.save canvas)
               child-size (measure-child child ctx cs)
               left (int (- (* (:width cs) coeff)
                            (* (:width child-size) child-coeff)))]
           ;; (clojure.pprint/pprint [cs (:width cs) child-size (:width child-size) left coeff
           ;;                         (- (:width cs) left (:width child-size))])
           (set! child-rect (cui/offset-lw cs left (:width child-size)))
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
  ([child-coeff coeff child] (->HAlign child-coeff coeff child nil)))

(deftype+ VAlign [child-coeff coeff child ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
            (measure-child child ctx cs))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [layer (.save canvas)
               child-size (measure-child child ctx cs)
               top (int (- (* (:height cs) coeff) (* (:height child-size) child-coeff)))]
           (set! child-rect (cui/offset-th cs top (:height child-size)))
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
  ([child-coeff coeff child] (->VAlign child-coeff coeff child nil)))

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

(deftype+ Column [children ^:mut child-rects]
  IComponent
  (-measure [_ ctx cs]
            (let [{:keys [width height]}
                  (reduce
                   (fn [{:keys [width y]} child]
                     (let [child-size (measure-child child ctx (cui/offset-lt cs 0 (min y (- (:bottom cs) (:y cs)))))]
                       (IPoint. (max width (:width child-size)) (+ y (:height child-size)))))
                   (IPoint. 0 0)
                   (keep #(nth % 2) children))]
              (cui/rect-with-wh cs width height)))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [known (for [[mode _ child] children]
                       (when (= :hug mode)
                         (measure-child child ctx cs)))
               space (max 0 (- (:height cs) (transduce (keep :height) + 0 known)))
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
                                    :stretch (IPoint. (:width cs) (-> space (/ stretch) (* value) (math/round))))
                       ;; _ (prn)
                       ;; _ (prn height (:height child-size) cs)
                       rect (cui/offset-th cs height (:height child-size))]
                   (when child
                     (draw-child child ctx rect canvas))
                   (.translate canvas 0 (:height child-size))
                   (recur
                    (min (+ height (long (:height child-size)))
                         #_(- (:bottom cs) (:y cs)))
                    (conj rects rect)
                    (next known)
                    (next children)))
                 (set! child-rects rects)))
             (finally (.restoreToCount canvas layer)))))

  (-event [_ event]
          (reduce
           (fn [acc [[_ _ child] rect]]
             (hui/eager-or acc (cui/event-propagate event child rect)))
           false
           (hui/zip children child-rects)))

  AutoCloseable
  (close [_]
         (doseq [[_ _ child] children]
           (ui/child-close child))))

(defn flatten-container* [children]
  (eduction (mapcat
              #(cond
                 (nil? %) []
                 (vector? %) [%]
                 (sequential? %) (flatten-container* %)
                 :else [[:hug nil %]]))
             children))

(defn flatten-container [children]
  (persistent!
   (enc/into! (transient [])
              (flatten-container* children))))

(defn column [& children]
  (cui/dyncomp (->Column (flatten-container children) nil)))

(deftype+ Row [children ^:mut child-rects ^boolean subpixel?]
  IComponent
  (-measure
   [_ ctx cs]
   (let [{:keys [width height]}
         (reduce
          (fn [{:keys [x height]} child]
            (let [child-size (measure-child
                              child ctx (cui/offset-lt cs (min x (- (:right cs) (:x cs))) 0))]
              (IPoint. (+ x (:width child-size)) (max height (:height child-size)))))
          (IPoint. 0 0)
          (keep #(nth % 2) children))]
     (cui/rect-with-wh cs width height)))

  (-draw [_ ctx cs ^Canvas canvas]
         (let [known (for [[mode _ child] children]
                       (when (= :hug mode)
                         (measure-child child ctx cs)))
               space (max 0 (- (:width cs) (transduce (keep :width) + 0 known)))
               stretch (transduce (keep (fn [[mode value _]] (when (= :stretch mode) value))) + 0 children)
               layer (.save canvas)
               max-dx (:width cs)]
           (loop [dx 0
                  rects []
                  known known
                  children children]
             (if-some [[mode value child] (first children)]
               (let [child-size (case mode
                                  :hug (first known)
                                  :stretch (if subpixel?
                                             (Point. (-> space (/ stretch) (* value)) (:height cs))
                                             (IPoint. (-> space (/ stretch) (* value) (math/round)) (:height cs))))
                     child-width (:width child-size)
                     rect (cui/offset-ltrb cs (min dx max-dx) 0 (max 0 (- max-dx child-width dx)) 0)]
                 (when child
                   (draw-child child ctx rect canvas))
                 (.translate canvas child-width 0)
                 (recur
                  (min (+ dx (long child-width))
                       #_(- (:right cs) (:x cs)))
                  (conj rects rect)
                  (next known)
                  (next children)))
               (set! child-rects rects)))
           (.restoreToCount canvas layer)))

  (-event [_ event]
          (reduce
           (fn [acc [[_ _ child] rect]]
             (hui/eager-or acc (cui/event-propagate event child rect) false))
           false
           (hui/zip children child-rects)))

  AutoCloseable
  (close [_]
         (doseq [[_ _ child] children]
           (ui/child-close child))))

(defn row [& children]
  (cui/dyncomp
   (->Row (flatten-container children) nil false)))

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
               layer (.save canvas)
               width' (max 0 (- (:width cs) left' right'))
               height' (max 0 (- (:height cs) top' bottom'))]
           ;; (prn left top right bottom cs left' right' top' bottom')
           (set! child-rect (cui/offset-ltrb cs left' top' right' bottom'))
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

(deftype+ VScroll [child ^:mut offset ^:mut size ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
            (let [child-cs (cui/unbounded-bottom cs)]
              (set! child-rect (cui/rect-with-wh cs (measure-child child ctx child-cs)))
              (IPoint. (:width child-rect) (:height cs))))

  (-draw [_ ctx cs ^Canvas canvas]
         (set! size cs)
         (set! offset (hui/clamp offset (- (:height cs) (int (:height child-rect))) 0))
         (let [layer (.save canvas)
               child-cs (cui/unbounded-bottom (cui/rect-translate cs 0 offset))]
           (try
             (.clipRect canvas (Rect/makeXYWH 0 0 (:width cs) (:height cs)))
             (.translate canvas 0 offset)
             (draw-child child ctx child-cs canvas)
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
                           event child (cui/rect-with-wh
                                        (cui/rect-translate size 0 offset))))))

  AutoCloseable
  (close [_]
         (ui/child-close child)))

(defn vscroll [child]
  (->VScroll child 0 nil nil))

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
         (let [layer (.save canvas)]
           (try
             (.clipRect canvas (Rect/makeXYWH 0 0 (:width cs) (:height cs)))
             (.translate canvas offset 0)
             (draw-child child ctx child-rect canvas)
             (finally
               (.restoreToCount canvas layer)))))

  (-event [_ event] (cui/event-propagate event child child-rect))

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
