(ns chic.digger.inspector
  (:require
   [clojure.string :as str]
   [chic.ui :as cui]
   [chic.ui.error :as cui.error]
   [chic.ui.event :as uievt]
   [clojure.reflect :as reflect]
   [chic.digger.inspector.throwable :as inspector.throwable]
   [chic.digger.inspector.thread-shapshot :as inspector.thread-shapshot]
   [io.github.humbleui.profile :as profile]
   [chic.ui.icons.material :as maticons]
   [io.github.humbleui.paint :as huipaint]
   [chic.ui.layout :as cuilay]
   [chic.ui.svg :as ui.svg]
   [chic.windows :as windows]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.ui :as ui]
   [taoensso.encore :as enc])
  (:import
   (java.lang.reflect Method Field)
   [io.github.humbleui.skija Paint Shader Canvas]
   [io.github.humbleui.types IPoint Point Rect]))

(defn class-hierarchy [cls]
  (loop [hierarchy []
         prev-class cls]
    (if-let [bs (bases prev-class)]
      (recur (conj hierarchy {:aclass (first bs)
                              :interfaces (vec (next bs))})
             (first bs))
      hierarchy)))

(defn class-hierarchy-view [obj]
  (ui/dynamic
   ctx [{:keys [fill-text font-ui]} ctx]
   (cuilay/column
    (map-indexed
     (fn [i {:keys [^Class aclass interfaces]}]
       (cuilay/padding
        (* i 5) 3 0 3
        (cuilay/row
         (ui/label (.getName aclass) font-ui fill-text)
         (for [itf interfaces]
           (cuilay/padding
            4 0 0 0
            (ui/label (.getName ^Class itf) font-ui (huipaint/fill 0xFFa0a0a0)))))))
     (reverse (class-hierarchy (class obj)))))))

(defn class-view [obj]
  (let [reflection (reflect/reflect obj)
        fade-fill (huipaint/fill 0xFFa0a0a0)
        fields (sort-by
                (fn [^Field f]
                  [(not (java.lang.reflect.Modifier/isStatic (.getModifiers f)))
                   (.getName f)])
                (.getFields (class obj)))
        mthds (.getMethods (class obj))
        mthds-by-class (group-by #(.getDeclaringClass ^Method %) mthds)
        mthds-by-class (into {}
                             (map (fn [[cls mthds]]
                                    [cls
                                     (sort-by (fn [^Method m]
                                                [(not (java.lang.reflect.Modifier/isStatic (.getModifiers m)))
                                                 (.getName m)])
                                              mthds)]))
                             mthds-by-class)
        last-seg #(last (str/split (str %) #"\."))]
    (ui/dynamic
     ctx [{:keys [fill-text font-ui]} ctx]
     (cuilay/column
      (cuilay/padding
       7 (ui/label (.getName (class obj)) font-ui fill-text))
      (cuilay/padding
       7 (ui/label (apply str "Flags  " (:flags reflection)) font-ui fill-text))
      (cuilay/hscroll
       (cuilay/padding
        7 0 (cui/dyncomp (class-hierarchy-view obj))))
      (ui/gap 0 5)
      (cuilay/padding
       7 3 (ui/label "Fields:" font-ui fill-text))
      (cuilay/hscroll
       (cuilay/column
        (for [^Field field fields]
          (cuilay/row
           (cuilay/padding
            7 3 (cuilay/row
                 (ui/label (str (when (java.lang.reflect.Modifier/isStatic (.getModifiers field))
                                  "static ")
                                (.getType field) " ") font-ui fade-fill)
                 (ui/label (str (.getName field) " = " (.get field obj)) font-ui fill-text)))
           (cuilay/valign
            0.5 (cui/clickable
                 (uievt/on-primary-down
                  (fn [evt]
                    (cui/emit evt [[::inspect-result (.get field obj)]])))
                 (ui/fill (huipaint/fill 0xFFe0e0e0)
                          (ui/gap 20 15))))))))
      (ui/gap 0 5)
      (cuilay/padding
       7 3 (ui/label "Methods:" font-ui fill-text))
      (cuilay/hscroll
       (cuilay/column
        (for [[^Class cls mthds] mthds-by-class]
          (cuilay/column
           (cuilay/padding
            7 8 7 5 (ui/label (str "From: " (.getName cls)) font-ui fill-text))
           (ui/fill (huipaint/fill 0x30000000) (ui/gap 0 1))
           (for [^Method method mthds]
             (cuilay/row
              (cuilay/padding
               7 5 (cuilay/row
                    (ui/label (str (when (java.lang.reflect.Modifier/isStatic (.getModifiers method))
                                     "static ")
                                   (last-seg (.getName (.getReturnType method))) " ")
                              font-ui fade-fill)
                    (ui/label (str (.getName method)
                                   "("
                                   (apply str (interpose ", " (map #(last-seg (.getName ^Class %)) (.getParameterTypes method))))
                                   ")")
                              font-ui fill-text)))
              (when (== 0 (count (.getParameterTypes method)))
                (cuilay/valign
                 0.5 (cui/clickable
                      (uievt/on-primary-down
                       (fn [evt]
                         (cui/emit evt [[::inspect-result
                                         @(future
                                            (try (clojure.lang.Reflector/invokeInstanceMethod
                                                  obj (.getName method) (make-array Object 0))
                                                 (catch Throwable e
                                                   e)))]])))
                      (ui/fill (huipaint/fill 0xFFe0e0e0)
                               (ui/gap 20 15)))))))))))))))

(defn list-view [obj]
  (ui/dynamic
   ctx [{:keys [fill-text font-code]} ctx]
   (cuilay/column
    (cuilay/row
     (cuilay/width
      40 (cuilay/halign
          1 (ui/label "Idx" font-code fill-text)))
     (ui/gap 5 0)
     (ui/label "Item" font-code fill-text))
    (map-indexed
     (fn [idx item]
       (cui/clickable
        (uievt/on-primary-down
         (fn [evt] (cui/emit evt [[::inspect-result item]])))
        (ui/dynamic
         ctx [{:hui/keys [hovered?]} ctx]
         (ui/fill
          (huipaint/fill
           (if hovered? 0x10000000
               0x00000000))
          (cuilay/padding
           0 3
           (cuilay/row
            (cuilay/width
             40 (cuilay/halign
                 1 (ui/label (str idx) font-code fill-text)))
            (ui/gap 5 0)
            (ui/label (str item) font-code fill-text)))))))
     obj))))

(defn dictionary-view [obj]
  (ui/dynamic
   ctx [{:keys [fill-text font-code]} ctx]
   (cuilay/column
    (cuilay/row
     [:stretch 1 (ui/clip (ui/label "Key" font-code fill-text))]
     (ui/gap 5 0)
     [:stretch 1 (ui/clip (ui/label "Value" font-code fill-text))])
    (let [item-ui
          (fn [item]
            (cui/clickable
             (uievt/on-primary-down
              (fn [evt] (cui/emit evt [[::inspect-result item]])))
             (ui/dynamic
              ctx [{:hui/keys [hovered?]} ctx]
              (ui/fill
               (huipaint/fill (if hovered? 0x10000000
                                  0x00000000))
               (ui/clip (cuilay/padding
                         0 3 (ui/label (str (or item "nil")) font-code fill-text)))))))]
      (map
       (fn [[k v]]
         (cuilay/row
          [:stretch 1 (item-ui k)]
          (ui/gap 5 0)
          [:stretch 1 (item-ui v)]))
       (sort-by key obj))))))

(defn get-inspector-views [obj]
  (->
   []
   (into
    (cond
      (:chic.debug/type.thread-snapshot-frames (meta obj))
      [{:title "Frames"
        :id :stack-frames-snapshot
        :ui-fn #(cui/dyncomp (inspector.thread-shapshot/frames-inspector-view %))}]
      (:chic.debug/type.stack-frame-variables (meta obj))
      [{:title "Variables"
        :id :stack-frames-variables
        :ui-fn #(cui/dyncomp (inspector.thread-shapshot/frame-variables-inspector-view %))}]))
   (into
    (cond
      (or (sequential? obj) (.isArray ^Class (type obj)))
      [{:title "List"
        :id :list
        :ui-fn #(cui/dyncomp (list-view (vec %)))}]
      (map? obj)
      [{:title "Map"
        :id :map
        :ui-fn #(cui/dyncomp (dictionary-view %))}]
      (instance? java.util.HashMap obj)
      [{:title "Map"
        :id :map
        :ui-fn #(cui/dyncomp (dictionary-view (into {} %)))}]
      (instance? Throwable obj)
      [{:title "Trace"
        :id :throwable-stack-trace
        :ui-fn #(cui/dyncomp (inspector.throwable/stack-trace-inspector-view %))}]))))

(defn inspector [obj]
  (cui.error/bound-errors
   (ui/dynamic
     ctx [{:keys [fill-text font-ui]} ctx
          _ @#'get-inspector-views]
     (let [views (conj (get-inspector-views obj)
                       {:title "Obj"
                        :id :java-object
                        :ui-fn (fn [obj] (cui/dyncomp (class-view obj)))})
           views (cond-> views (meta obj)
                         (conj {:title "Meta"
                                :id :metadata
                                :ui-fn #(inspector (meta %))}))
           *state (atom {:selected-view (:id (first views))})]
       (ui/dynamic
         ctx [{:keys [selected-view]} @*state]
         (cuilay/column
          (ui/fill (huipaint/fill 0xFFd9d9d9)
                   (cuilay/hscroll
                    (cuilay/padding
                     5 (ui/label (str obj) font-ui fill-text))))
          (ui/fill
           (huipaint/fill 0xFFFFFFFF)
           (cond
             (nil? obj)
             (cuilay/padding
              10 (ui/label "nil" font-ui fill-text))
             :else
             (cuilay/column
              (cuilay/hscroll
               (cuilay/row
                (for [v views]
                  (cui/clickable
                   (uievt/on-primary-down
                    (fn [_] (swap! *state assoc :selected-view (:id v))))
                   (cuilay/padding
                    2 (ui/clip-rrect
                       5 (ui/fill
                          (huipaint/fill (if (= (:id v) selected-view)
                                           0x6070b0c0
                                           0x10004050))
                          (cuilay/padding
                           4 (ui/label (:title v) font-ui fill-text)))))))))
              (cuilay/vscroll
               (some (fn [v] (when (= selected-view (:id v))
                               ((:ui-fn v) obj))) views)))))))))))

(comment

  (tap> (repeat 50 4))
  (sequential? (repeat 50 4))
  (type (bases (class {})))
  (into #{} (map :declaring-class (:members (reflect/reflect {}))))
  (tap> (.getMethod (class {}) "clear" (make-array Class 0)))
  (tap> (Point. 1 3))
  (tap> (.get (second (seq (.getFields (class (Point. 0 0))))) (Point. 0 0)))
  (ancestors (class {}))
  (supers (class (tap> {})))
  #!
  )
