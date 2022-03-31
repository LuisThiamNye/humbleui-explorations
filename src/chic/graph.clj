(ns chic.graph
  (:require
   [chic.ui :as cui]
   [chic.ui.svg :as ui.svg]
   [chic.ui.icons.material :as maticons]
   [chic.ui.layout :as cuilay]
   [chic.windows :as windows]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.ui :as ui]
   [taoensso.encore :as enc])
  (:import
   [io.github.humbleui.skija Paint Shader]
   [io.github.humbleui.types IPoint Point]))

(defn vadd [a b] (Point. (unchecked-add (:x a) (:x b)) (unchecked-add (:y a) (:y b))))
(defn vsub [a b] (Point. (unchecked-subtract (:x a) (:x b)) (unchecked-subtract (:y a) (:y b))))

(let [disp -40]
  (cond
    (< disp -50)
    0
    (< disp 50)
    (* (+ disp 50) 2)
    :else
    (* 2 50)))

(defn particle-graph-do-physics [nodes *state *draw-state]
  (let [vscale (fn [s v] (Point. (unchecked-multiply s (:x v)) (unchecked-multiply s (:y v))))
        vdot (fn [a b] (unchecked-add (unchecked-multiply (:x a) (:x b)) (unchecked-multiply (:y a) (:y b))))
        vabs (fn [v] (math/sqrt (vdot v v)))
        {:keys [container-size]
         {:keys [time-scale attraction-coeff friction-coeff
                 repulsion-coeff repel-range attraction-deadzone sorting-buffer
                 min-velocity max-velocity
                 compression? repel-connected? distribute-spring?]}
         :physics-params} @*state
        ;; time-scale 0.001 ;; ms/s work in pixels/second
        max-dt (* time-scale 100)
        min-velocity (/ min-velocity time-scale)
        max-velocity (/ max-velocity time-scale)
        min-accel (/ 0.000001 time-scale)
        max-accel (/ 1 time-scale)
        dist->acc-attract (fn [dist] (cond-> (* attraction-coeff (- dist attraction-deadzone))
                                       (not compression?)
                                       (max 0)))
        friction-acc (fn [vel] (vscale (- friction-coeff) vel))
        ;; (/ 1 (math/pow (unchecked-add 100 5) 2))  ;
        ;; dist->acc-repel (fn [dist] (min 0 (* 1000000 (unchecked-subtract 9e-5 (/ 1 (math/pow (unchecked-add dist 5) 2.))))))
        ;; repel-range-coeff (/ 1 (math/pow (unchecked-add repel-range 5) 2.))
        repel-range-coeff (/ 1 (unchecked-add repel-range 5))
        dist->acc-repel (fn [dist]
                          (min 0 (* repulsion-coeff
                                    (unchecked-subtract repel-range-coeff
                                                        (/ 1 (unchecked-add dist 5))))))
        disp->sorting-acc (fn [disp]
                            (* 100 (cond
                                     (< disp (- sorting-buffer))
                                     0
                                     (< disp sorting-buffer)
                                     (* (+ sorting-buffer disp) 2)
                                     :else
                                     (* 2 2 50))))
        t2 (System/currentTimeMillis)]
    (vswap! *draw-state
            (fn [{:keys [t] vnodes :nodes :as ds}]
              (let [dt (min max-dt (* time-scale (unchecked-subtract t2 t)))
                    {:keys [width height]} container-size]
                (reduce
                 (fn [ds [id {:keys [position velocity] :as vnode}]]
                   (if (and position (not (contains? (:dragging-nodes @*state) id)))
                     (let [connected-nodes (:connected-nodes (get nodes id))
                           *accel (volatile! (friction-acc velocity))]
                       (doseq [kv (dissoc vnodes id)]
                         (let [other-vnode-id (key kv)
                               other-vnode (val kv)
                               diff (vsub (:position other-vnode) position)
                               dist (vabs diff)
                               diff-unit (if (< dist 1)
                                           (IPoint. (dec (* 2 (rand-int 2))) (dec (* 2 (rand-int 2))))
                                           (vscale (/ dist) diff))
                               sending-connection? (contains? connected-nodes other-vnode-id)
                               receiving-connection? (contains? (:connected-nodes (get nodes other-vnode-id)) id)
                               connected-to-this-node? (or sending-connection? receiving-connection?)
                               total-node-count (+ (count connected-nodes)
                                                   (count (:connected-nodes (get nodes other-vnode-id))))
                               disp (:y diff)]
                           (vswap! *accel
                                   #(-> %
                                        (cond->
                                          (or repel-connected? (not connected-to-this-node?))
                                          (vadd (vscale (dist->acc-repel dist) diff-unit)))
                                        (cond-> connected-to-this-node?
                                          (-> (vadd (vscale (cond-> (dist->acc-attract dist)
                                                              distribute-spring?
                                                              (/ total-node-count)) diff-unit))
                                              (vadd (vscale (if receiving-connection?
                                                              (disp->sorting-acc disp)
                                                              (- (disp->sorting-acc (- disp)))) (Point. 0 1)))))))))
                       (let [accel @*accel
                             accel-mag (vabs accel)
                             accel-mag' (min max-accel accel-mag)
                             accel (if (< min-accel accel-mag')
                                     (vscale (/ accel-mag' accel-mag) accel) (Point. 0 0))
                             v2 (vadd velocity (vscale dt accel))
                             v2-mag (vabs v2)
                             v2-mag' (min max-velocity v2-mag)
                             v2 (if (< min-velocity v2-mag')
                                  (vscale (/ v2-mag' v2-mag) v2) (Point. 0 0))
                             p2 (vadd position (vscale dt v2))
                             v2 (-> v2
                                    (cond-> (< (:x p2) 0)
                                      (.withX (abs (:x v2))))
                                    (cond-> (< width (:x p2))
                                      (.withX (- (abs (:x v2)))))
                                    (cond-> (< (:y p2) 0)
                                      (.withY (abs (:y v2))))
                                    (cond-> (< height (:y p2))
                                      (.withY (- (abs (:y v2))))))
                             p2 (Point. (hui/clamp (:x p2) 0 width)
                                        (hui/clamp (:y p2) 0 height))]
                         (assoc-in
                          ds [:nodes id]
                          (-> vnode
                              (assoc :velocity v2)
                              (assoc :position p2)))))
                     ds))
                 (assoc ds :t t2)
                 vnodes))))))

(defn circle-shape [diameter paint]
  (ui/custom-ui
   diameter diameter
   {:on-paint (fn [canvas width height]
                (let [radius (min (/ width 2) (/ height 2))]
                  (.drawCircle canvas radius radius radius paint)))}))

(defn particle-graph* [{:keys [*state *draw-state]}]
  (let [radius 7
        line-paint (doto (Paint.)
                     (.setStrokeWidth 1.5)
                     (.setShader (Shader/makeLinearGradient
                                  0. 0. 70. 0.
                                  (int-array 2 [(unchecked-int 0xFF30c0c0)
                                                (unchecked-int 0xFF10c0e0)])))
                     #_(.setColor (unchecked-int 0xFF30c0c0)))]
    (cui/clickable
     (fn [event]
       (when (and (not (:hui.event.mouse-button/is-pressed event))
                  (seq (:dragging-nodes @*state)))
         (swap! *state assoc :dragging-nodes #{})))
     (cui/on-mouse-move
      (fn [event]
        (when-let [ids (seq (:dragging-nodes @*state))]
          (vswap! *draw-state
                  (fn [ds]
                    (reduce
                     (fn [ds id]
                       (assoc-in ds [:nodes id :position]
                                 (let [p (vadd (:mouse-drag-offset @*state)
                                               (:chic.ui/mouse-win-pos event))
                                       {:keys [width height]} (:container-size @*state)]
                                   (Point. (hui/clamp (:x p) 0 width)
                                           (hui/clamp (:y p) 0 height)))))
                     ds ids)))))
      (ui/dynamic
       ctx [nodes (:nodes @*state)
            _ (:physics-params @*state)
            vnodes (:nodes @*draw-state)
            window (:chic/current-window ctx)
            {:keys [font-ui fill-text]} ctx]
       (some-> (:timeout @*draw-state) future-cancel)
       (vswap! *draw-state assoc :timeout
               (enc/after-timeout 14
                                  (try (particle-graph-do-physics nodes *state *draw-state)
                                       (catch Exception e
                                         (prn e)))
                                  (windows/request-frame window)))
       (cuilay/size-dependent
        (fn [cs]
          (cui/on-draw
           (fn [_ctx cs _canvas]
             (swap! *state assoc :container-size
                    (Point. (- (:x cs) (* 2 radius)) (- (:y cs) (* 2 radius)))))
           (cuilay/stack
            (cuilay/padding
             radius
             (cuilay/stack
              (eduction
               (map (fn [[id node]]
                      (let [pos1 (:position (get vnodes id))]
                        (eduction
                         (map (fn [connected-id]
                                (ui/custom-ui
                                 (:width cs) (:height cs)
                                 {:on-paint (fn [canvas _width _height]
                                              (let [pos2 (:position (get vnodes connected-id))]
                                                (.drawLine canvas
                                                           (:x pos1) (:y pos1)
                                                           (:x pos2) (:y pos2)
                                                           (doto (Paint.)
                                                             (.setStrokeWidth 1.5)
                                                             (.setShader (Shader/makeLinearGradient
                                                                          (float (:x pos1)) (float (:y pos1))
                                                                          (float (:x pos2)) (float (:y pos2))
                                                                          (int-array 2 [(unchecked-int 0xC00047ff)
                                                                                        (unchecked-int 0xC0ff9900)])))))))})))
                         (:connected-nodes node)))))
               nodes)))
            (eduction
             (map (fn [[id node]]
                    (when-not (contains? (:nodes @*draw-state) id)
                      (let [#_#_{:keys [width height]} (:container-size @*state)]
                        (vswap! *draw-state assoc-in [:nodes id]
                                {:velocity (Point. 0 0)
                                 :position #_(Point. (rand-int width) (rand-int height))
                                 (Point. 0 0)})))
                    (let [{:keys [position]} (get-in @*draw-state [:nodes id])]
                      (let [{:keys [x y]} position]
                        (cuilay/halign
                         0 (cuilay/valign
                            0 (cuilay/padding
                               #(hui/clamp x 0 (:width %))
                               #(hui/clamp y 0 (:height %)) 0 0
                               (cui/clickable
                                (fn [event]
                                  (when (and (:hui.event.mouse-button/is-pressed event)
                                             (empty? (:dragging-nodes @*state)))
                                    (swap! *state (fn [state]
                                                    (-> state
                                                        (assoc :dragging-nodes #{id})
                                                        (assoc :mouse-drag-offset (vsub (get-in @*draw-state [:nodes id :position])
                                                                                        (:chic.ui/mouse-win-pos event))))))
                                    (vswap! *draw-state (fn [ds]
                                                          (update-in ds [:nodes id] assoc :velocity (Point. 0 0))))))
                                (circle-shape (* 2 radius)
                                              (doto (Paint.) (.setColor (unchecked-int 0xFF5077ff#_0xFF10b0b0))))))))))))
             nodes)
            (eduction
             (map (fn [[id node]]
                    (let [{:keys [position]} (get-in @*draw-state [:nodes id])
                          {:keys [x y]} position
                          child (ui/fill
                                 (doto (Paint.) (.setColor (unchecked-int 0xa0FFFFFF)))
                                 (ui/height
                                  (* 2 radius)
                                  (ui/valign 0.5 (ui/label (:label node) font-ui fill-text))))
                          rhs-offset (+ (* 2 radius) 2 x)]
                      (cuilay/halign
                       0 (cuilay/valign
                          0 (cuilay/size-dependent
                             (fn [cs]
                               (ui/contextual
                                (fn [ctx]
                                  (let [{child-width :width
                                         child-height :height} (cui/measure-child child ctx cs)
                                        xoffset (if (<= (+ rhs-offset child-width) (:width cs))
                                                  (hui/clamp rhs-offset 0 (:width cs))
                                                  (hui/clamp (- x child-width 2) 0 (:width cs)))]
                                    #_(if (= id 1437244129)
                                        (prn rhs-offset child-width (:width cs)))
                                    (cuilay/padding
                                     xoffset
                                     (hui/clamp y 0 (- (:height cs) child-height))
                                     (- (:width cs) xoffset child-width) 0
                                     child)))))))))))
             nodes))))))))))

(defn slider [*vstate {:keys [value on-change]}]
  (let [cursor-width 12
        value (hui/clamp value 0 1)]
    (cui/on-event
     (fn [event]
       (when (and (= (:hui/event event) :hui/mouse-button)
                  (:dragging? @*vstate)
                  (not (:hui.event.mouse-button/is-pressed event)))
         (vswap! *vstate assoc :dragging? false)))
     (cuilay/height
      20
      (cuilay/stack
       (cuilay/valign
        0.5 (ui/fill (Paint.) (ui/gap 0 3)))
       (cuilay/valign
        0.5 (cuilay/size-dependent
             (fn [cs]
               (cui/on-mouse-move
                (fn [event]
                  (when (:dragging? @*vstate)
                    (let [new-pos (:x (cui/component-relative-pos event (:chic.ui/mouse-win-pos event)))
                          ;; diff (/ (- new-pos (:last-mouse-pos @*vstate)) (:width cs))
                          ]
                      ;; (vswap! *vstate assoc :last-mouse-pos new-pos)
                      (on-change (hui/clamp (/ (+ new-pos (:mouse-offset @*vstate))
                                               (- (:width cs) cursor-width)) 0 1)))))
                (cuilay/halign
                 value
                 (cui/clickable
                  (fn [event]
                    (when (and (not (:dragging? @*vstate))
                               (:hui.event.mouse-button/is-pressed event))
                      (vswap! *vstate assoc :dragging? true
                              :mouse-offset
                              (- (:x (cui/component-relative-pos
                                      event (:chic.ui/mouse-win-pos event)))))))
                  (circle-shape cursor-width (Paint.)))))))))))))

(defn checkbox [{:keys [checked? on-change]}]
  (cui/clickable
   (fn [event]
     (when (:hui.event.mouse-button/is-pressed event)
       (on-change (not checked?))))
   (ui/clip-rrect
    4
    (if checked?
      (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFF0090FF)))
               (ui.svg/make (maticons/find-svg-file "check" "round" "24px")))
      (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
               (ui/padding
                2 (ui/clip-rrect
                   2 (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFFFFFFF)))
                            (ui/gap 0 0)))))))))

(defn initial-slider-state []
  (volatile! {:dragging? false}))

(defn particle-graph-with-controls [{:keys [*state] :as model}]
  (ui/dynamic
   ctx [{:keys [font-ui fill-text]} ctx]
   (let [aslider
         (fn [paramkey {:keys [min-limit max-limit label state]}]
           (let [state (initial-slider-state)]
             [:stretch 1
              (ui/dynamic
               _ [param (get (:physics-params @*state) paramkey)]
               (cuilay/padding
                1 0
                (cuilay/column
                 (ui/gap 0 3)
                 (ui/label (str label ": "
                                (if (float? param)
                                  (format "%.3f" param)
                                  (str param))) font-ui fill-text)
                 (cui/dyncomp
                  (slider
                   state
                   {:value (if (zero? min-limit)
                             (/ (unchecked-subtract param min-limit) (unchecked-subtract max-limit min-limit))
                             (/ (math/log (/ param min-limit))
                                (math/log (/ max-limit min-limit))))
                    :on-change (fn [value]
                                 (swap! *state assoc-in [:physics-params paramkey]
                                        (if (zero? min-limit)
                                          (unchecked-add min-limit
                                                         (unchecked-multiply (float value) (- max-limit min-limit)))
                                          (* min-limit (math/pow (/ max-limit min-limit)
                                                                 (float value))))))})))))]))
         acheckbox
         (fn [paramkey {:keys [label]}]
           (ui/dynamic
             _ [checked? (get (:physics-params @*state) paramkey)]
             (cui/dyncomp
             (cuilay/padding
              1 0 (ui/valign
                   0.5 (cuilay/row
                        (ui/width
                         18 (ui/height
                             18 (checkbox
                                 {:checked? checked?
                                  :on-change (fn [checked?]
                                               (swap! *state assoc-in [:physics-params paramkey] checked?))})))
                        (ui/padding
                         5 4 (ui/label label font-ui fill-text))))))))
         speed-slider-state (initial-slider-state)
         attraction-slider-state (initial-slider-state)
         repulsion-slider-state (initial-slider-state)
         friction-slider-state (initial-slider-state)]
     (cuilay/column
      [:stretch 1
       (cui/dyncomp (particle-graph* model))]
      (ui/gap 0 5)
      (cuilay/row
       (acheckbox :compression? {:label "compression"})
       (acheckbox :repel-connected? {:label "repel connected"})
       (acheckbox :distribute-spring? {:label "distribute spring"}))
      (cuilay/row
       (aslider :attraction-coeff {:label "stiffness" :min-limit 1 :max-limit 1000
                                   :state attraction-slider-state})
       (aslider :attraction-deadzone {:label "natural length" :min-limit 01 :max-limit 1000})
       (aslider :repulsion-coeff {:label "repulsion" :min-limit 100 :max-limit 100000000
                                  :state repulsion-slider-state})
       (aslider :repel-range {:label "repel-range" :min-limit 0 :max-limit 1000}))
      (cuilay/row
       (aslider :time-scale {:label "speed" :min-limit 0.001 :max-limit 0.01
                             :state speed-slider-state})
       (aslider :friction-coeff {:label "friction" :min-limit 0.1 :max-limit 100
                                 :state friction-slider-state})
       (aslider :sorting-buffer {:label "sorting buffer" :min-limit 0 :max-limit 500
                                 :state friction-slider-state}))
      (cuilay/row
       (aslider  :min-velocity{:label "min velocity" :min-limit 0.001 :max-limit 1})
       (aslider :max-velocity  {:label "max velocity" :min-limit 0 :max-limit 10}))))))

(comment
  (:nodes @--ds)

  (def sample-nodes
    {0 {:label "Alice"
        :connected-nodes #{1 2}}
     1 {:label "Bob"
        :connected-nodes #{1}}
     2 {:label "Charlie"
        :connected-nodes #{3 4}}
     3 {:label "Charlie"
        :connected-nodes #{2 4}}
     4 {:label "Charlie"
        :connected-nodes #{1}}})
#!
  )
(defn particle-graph-new-state [nodes]
  (let [*state (atom {:nodes nodes
                      :mouse-drag-offset nil
                      :dragging-nodes #{}
                      :physics-params {:time-scale 0.001
                                       :attraction-coeff 10
                                       :friction-coeff 8
                                       :repulsion-coeff 1000000
                                       :attraction-deadzone 200
                                       :repel-range 120
                                       :min-velocity 0.001
                                       :max-velocity 1
                                       :sorting-buffer 70
                                       :compression? true
                                       :repel-connected? false
                                       :distribute-spring? true}})
        *draw-state (volatile! {:nodes {}
                                :t (System/currentTimeMillis)})]
    (def --ds *draw-state)
    {:*state *state :*draw-state *draw-state}))

(defn particle-graph [state]
  (cui/dyncomp (particle-graph-with-controls state)))
