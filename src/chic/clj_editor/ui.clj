(ns chic.clj-editor.ui
  (:require
   [potemkin :refer [doit]]
   [clj-commons.primitive-math :as prim]
   [chic.clj-editor.parser :as parser]
   [chic.util :as util]
   [chic.clj-editor :as clj-editor]
   [chic.clj-editor.ast.string :as ast.string]
   [chic.clj-editor.ast :as ast]
   [chic.clj-editor.lines :as lines]
   [rewrite-clj.parser :as rw.p]
   [rewrite-clj.node :as rw.n]
   [rewrite-clj.zip :as rw.z]
   [io.github.humbleui.profile :as profile]
   [chic.focus :as focus]
   [io.github.humbleui.paint :as huipaint]
   [chic.ui.layout :as cuilay]
   [chic.ui.focusable :as focusable]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.ui :as ui]
   [chic.ui :as cui])
  (:import
   (chic.text_editor TextEditor)
   (io.github.humbleui.skija.shaper ShapingOptions)
   [io.github.humbleui.skija Canvas Paint TextLine ColorFilter
    BlendMode ColorMatrix InversionMode FontMetrics Font]
   [io.github.humbleui.types IPoint IRect Rect RRect]
   [java.lang AutoCloseable]))

(deftype+ SegLabel [^java.util.ArrayList lines ^java.util.ArrayList paints ^Font font ^FontMetrics metrics]
  IComponent
  (-measure [_ ctx rect]
    (IPoint.
     (Math/ceil (reduce unchecked-add (unchecked-float 0)
                        (eduction (map #(.getWidth ^TextLine %)) lines)))
     (Math/ceil (.getCapHeight metrics))))
  (-draw [_ ctx rect ^Canvas canvas]
    (loop [i (unchecked-int 0)
           x (unchecked-float 0)]
      (when (prim/< i (.size lines))
        (let [line ^TextLine (.get lines i)
              paint ^Paint (.get paints i)]
          (.drawTextLine canvas line x (Math/ceil (.getCapHeight metrics)) paint)
          (recur (prim/inc i)
                 (unchecked-add x (.getWidth line)))))))
  (-event [_ evt])
  AutoCloseable
  (close [_]))

(def ^Paint cursor-blue (huipaint/fill 0xE0007ACC))

(defn seg-label [strs+paints ^Font font]
  (let [n (count strs+paints)
        paints (java.util.ArrayList. n)
        lines (java.util.ArrayList. n)
        cursor-width (unchecked-int 2)]
    (doit [[s paint] strs+paints]
      (.add paints paint)
      (.add lines (.shapeLine cui/shaper s font ShapingOptions/DEFAULT)))
    (cui/dynamic ctx
      [{:keys [seg-cursor-idx]} ctx]
      (let [ui (cui/responder
                {::cursor-rect->idx
                 (fn [child {:keys [rect]}]
                   (let [xstart (:x (:rect child))
                         x (:x rect)
                         cdx (- x xstart)]
                     (loop [i 0
                            dx 0
                            idx 0]
                       (if (< i (.size lines))
                         (let [line ^TextLine (.get lines i)
                               s (nth (nth strs+paints i) 0)
                               dx' (+ dx (.getWidth line))]
                           (if (< dx' cdx)
                             (recur (inc i) dx' (+ idx (count s)))
                             (+ idx (.getOffsetAtCoord line (- cdx dx)))))
                         idx))))}
                (cui/measured
                 (cuilay/valign
                  0.3 (cui/dyncomp
                       (->SegLabel lines paints font (.getMetrics font))))))]
        (if seg-cursor-idx
          (let [loffset (loop [idx seg-cursor-idx
                               i 0
                               dx 0]
                          (if (< i (.size lines))
                            (let [line ^TextLine (.get lines i)
                                  s (nth (nth strs+paints i) 0)
                                  idx' (- idx (count s))]
                              (if (neg? idx')
                                (+ dx (.getCoordAtOffset line idx))
                                (recur idx' (inc i) (+ dx (.getWidth line)))))
                            (- dx cursor-width)))]
            (cuilay/stack
             ui
             (cuilay/halign
              0 (cuilay/translate
                 loffset 0
                 (cui/responder
                  {::get-cursor-rect
                   (fn [child _] (:rect child))}
                  (cui/measured (ui/fill cursor-blue (ui/gap cursor-width 0))))))))
          ui)))))

(deftype+ VirtualVScroll [seg-ids ctor ^:mut child child-after ^:mut offset ^:mut size ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
    (cui/measure-child child ctx (cui/unbounded-bottom cs)))

  (-draw [_ ctx cs ^Canvas canvas]
    (set! size cs)
    (let [child-height (:height (cui/measure-child child ctx (cui/unbounded-bottom cs)))
          layer (.save canvas)]
      (set! offset (hui/clamp offset (- (:height cs) child-height) 0))
      (set! child-rect (cui/rect-translate
                        (cui/rect-with-wh cs (:width cs) (max (:height cs) child-height))
                        0 offset))
      (try
        (.clipRect canvas (Rect/makeXYWH 0 0 (:width cs) (:height cs)))
        (.translate canvas 0 offset)
        (cui/draw-child child (cui/push-point-clip
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

(defn virtual-vscroll [seg-ids ctor child-after]
  (cui/dyncomp (->VirtualVScroll seg-ids ctor (ui/gap 0 0) child-after 0 nil nil)))

(defn request [self msg arg]
  (let [responders ^java.util.HashMap (:responders self)
        responders-by-msg ^java.util.HashMap (:responders-by-msg self)]
    (when-some [child (.get responders-by-msg msg)]
      ((get (.get responders child) msg) child arg))))
