(ns chic.graph
  (:require
   [chic.ui :as cui]
   [chic.util :as util]
   [io.github.humbleui.profile :as profile]
   [chic.ui.icons.material :as maticons]
   [io.github.humbleui.paint :as huipaint]
   [chic.ui.layout :as cuilay]
   [chic.ui.svg :as ui.svg]
   [chic.ui.event :as uievt]
   [chic.windows :as windows]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.ui :as ui]
   [taoensso.encore :as enc])
  (:import
   [io.github.humbleui.skija Paint Shader Canvas]
   [io.github.humbleui.types IPoint Point Rect]))

(defn vadd [a b] (Point. (unchecked-add (:x a) (:x b)) (unchecked-add (:y a) (:y b))))
(defn vsub [a b] (Point. (unchecked-subtract (:x a) (:x b)) (unchecked-subtract (:y a) (:y b))))
(defn vscale [s v] (Point. (unchecked-multiply s (:x v)) (unchecked-multiply s (:y v))))

(let [disp -40]
  (cond
    (< disp -50)
    0
    (< disp 50)
    (* (+ disp 50) 2)
    :else
    (* 2 50)))

(defn particle-graph-do-physics [nodes *state *draw-state]
  (let [vdot (fn [a b] (unchecked-add (unchecked-multiply (:x a) (:x b)) (unchecked-multiply (:y a) (:y b))))
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
    (vswap!
     *draw-state
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
                      v2 (-> ^Point v2
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

(defn oval-shape [paint]
  #_(ui/custom-ui
     diameter diameter
     {:on-paint (fn [canvas width height]
                  (let [radius (/ diameter 2)]
                    (.drawCircle canvas radius radius radius paint)))})
  (cui/on-draw
   (fn [_ cs ^Canvas canvas]
     (.drawOval canvas (Rect/makeXYWH 0 0 (:width cs) (:height cs)) paint))
   (ui/gap 0 0)))

(def ^:dynamic *node-connect-trace* #{})

(def node-connects-to?*
  (enc/memoize
   (fn node-connects-to?** [nodes fromid toid trace]
     (when-not (contains? trace fromid)
       (let [trace (conj trace fromid)]
         (some (fn [childid]
                 (or (= childid toid)
                     (node-connects-to?* nodes childid toid trace)))
               (:connected-nodes (get nodes fromid))))))))
(defn node-connects-to? [nodes fromid toid]
  #_(when (contains? *node-connect-trace* fromid)
      (throw (ex-info "Circular dependency"
                      {:from (get nodes fromid)
                       :connected-nodes (map #(:label (get nodes %)) (:connected-nodes (get nodes fromid)))
                       :to (get nodes toid)
                       :trace (map #(:label (get nodes %)) *node-connect-trace*)})))
  (node-connects-to?* nodes fromid toid #{}))

(defn edge-view [id connected-nodes connected-id]
  (ui/dynamic
   ctx [{{:keys [view-config nodes]} ::state} ctx]
   (let [secondary? (and (:suppress-shortcuts? view-config)
                         (try (some #(node-connects-to? nodes % connected-id)
                                    (disj connected-nodes connected-id id))
                              (catch Throwable e
                                (println (get nodes id) (get nodes connected-id))
                                (throw e))))
         stroke (doto (Paint.) (.setStrokeWidth (if secondary? 1 1.5)))
         colours (int-array 2 (if secondary?
                                [(unchecked-int 0x400047ff)
                                 (unchecked-int 0x40ff9900)]
                                [(unchecked-int 0xD00047ff)
                                 (unchecked-int 0xD0ff9900)]))]
     (cui/on-draw
      (fn [{:keys [scale] :as ctx} _cs ^Canvas canvas]
        (let [vnodes (:nodes (::draw-state ctx))
              pos1 (:position (get vnodes id))
              pos2 (:position (get vnodes connected-id))
              stroke (.setShader
                      stroke (Shader/makeLinearGradient
                              (float (:x pos1)) (float (:y pos1))
                              (float (:x pos2)) (float (:y pos2))
                              colours))]
          (.drawLine canvas
                     (* scale (:x pos1)) (* scale (:y pos1))
                     (* scale (:x pos2)) (* scale (:y pos2)) stroke)))
      (ui/gap 0 0)))))

(defn node-view [id]
  (cui/updating-ctx
   (fn [ctx]
     (assoc ctx :vnode (get (:nodes (::draw-state ctx)) id)))
   (ui/dynamic
     ctx [{{:keys [radius]} ::state} ctx]
     (let [ui-node (cui/clickable
                    (uievt/on-primary-down
                     (fn [event]
                       (cui/emit event [[::start-dragging (assoc event :id id)]])))
                    (oval-shape (huipaint/fill 0xFF5077ff #_0xFF10b0b0)))]
       (cuilay/halign
        0 (cuilay/valign
           0 (cuilay/width
              (* 2 radius)
              (cuilay/height
               (* 2 radius)
               (ui/dynamic
                 ctx [{:keys [container-bounds]} ctx
                      {{:keys [x y]} :position} (:vnode ctx)]
                 (cuilay/translate
                  (hui/clamp x 0 (- (:width container-bounds) (* 2 radius)))
                  (hui/clamp y 0 (- (:height container-bounds) (* 2 radius)))
                  ui-node))))))))))

(defn annotation-view [id]
  (ui/dynamic
   ctx [{:keys [font-ui fill-text]
         {:keys [radius nodes]} ::state} ctx]
   (let [node (get nodes id)
         padding (unchecked-add (unchecked-multiply 2 radius) 2)
         child (cuilay/valign
                0.5 (ui/fill
                     (huipaint/fill 0xa0FFFFFF)
                     (cuilay/height
                      (* 2 radius)
                      (ui/label (:label node) font-ui fill-text))))]
     (cuilay/halign
      0 (cuilay/valign
         0 (ui/contextual
            (fn [ctx]
              (let [container-bounds (:container-bounds ctx)
                    {{:keys [x y]} :position} (get (:nodes (::draw-state ctx)) id)
                    rhs-offset (unchecked-add padding x)
                    {child-width :width
                     child-height :height} (cui/measure-child child ctx (:chic.ui/component-rect ctx))
                    container-width (:width container-bounds)
                    xoffset (min (if (<= (unchecked-add rhs-offset child-width) container-width)
                                   (max rhs-offset 0)
                                   (max (unchecked-subtract (unchecked-subtract x child-width) 2) 0))
                                 (unchecked-subtract (unchecked-subtract container-width (unchecked-multiply 2 radius))
                                                     2))]
                (cuilay/translate
                 xoffset
                 (hui/clamp y 0 (unchecked-subtract (:height container-bounds) child-height))
                 child)))))))))

(defn particle-graph** []
  (cui/updating-ctx
   (let [*node-ids (volatile! nil)]
     (fn [ctx]
       (let [node-ids (keys (:nodes (::state ctx)))
             prev @*node-ids]
         (if (= prev node-ids)
           (assoc ctx :node-ids prev)
           (assoc ctx :node-ids (vreset! *node-ids node-ids))))))
   (ui/dynamic
     ctx [{{:keys [radius]} ::state
           :keys [node-ids]} ctx]
     (cui/with-bounds
       :container-bounds
       (cuilay/stack
        (cuilay/padding
         radius
         (ui/dynamic
           ctx [{{:keys [nodes]} ::state} ctx]
           (cuilay/stack
            (eduction
             (map
              (fn [[id {:keys [connected-nodes]}]]
                (eduction
                 (map
                  (fn [connected-id]
                    (edge-view id connected-nodes connected-id)))
                 connected-nodes)))
             nodes))))
        (eduction (map (fn [id] (node-view id)))
                  node-ids)
        (eduction (map (fn [id] (annotation-view id)))
                  node-ids))))))

(defn particle-graph* [{:keys [*state *draw-state]}]
  (cui/updating-ctx
   (fn [ctx]
     (assoc ctx
            ::state @*state
            ::draw-state @*draw-state))
   (cui/on
    ::start-dragging
    (fn [{:keys [scale id] :as event}]
      (when (empty? (:dragging-nodes @*state))
        (swap! *state (fn [state]
                        (-> state
                            (assoc :dragging-nodes #{id})
                            (assoc :mouse-drag-offset (vsub (get-in @*draw-state [:nodes id :position])
                                                            (vscale (/ scale) (:chic.ui/mouse-win-pos event)))))))
        (vswap! *draw-state (fn [ds]
                              (update-in ds [:nodes id] assoc :velocity (Point. 0 0))))))
    (cui/on-mouse-move
     (fn [event]
       (when-let [ids (seq (:dragging-nodes @*state))]
         (vswap! *draw-state
                 (fn [ds]
                   (reduce
                    (fn [ds id]
                      (assoc-in ds [:nodes id :position]
                                (let [p (vadd (:mouse-drag-offset @*state)
                                              (vscale (/ (:scale event)) (:chic.ui/mouse-win-pos event)))
                                      {:keys [width height]} (:container-size @*state)]
                                  (Point. (hui/clamp (:x p) 0 width)
                                          (hui/clamp (:y p) 0 height)))))
                    ds ids)))))
     (cui/clickable
      (uievt/on-primary-up
       (fn [_]
         (when (seq (:dragging-nodes @*state))
           (swap! *state assoc :dragging-nodes #{}))))
      (cui/on-draw
       (fn [{:keys [scale ::state] :as ctx} {:keys [width height]} _canvas]
         (some-> (:timeout @*draw-state) future-cancel)
         (vswap! *draw-state assoc :timeout
                 (future
                   (try (particle-graph-do-physics (:nodes state) *state *draw-state)
                        (catch Exception e
                          (println (str "at: " (java.util.Date.)))
                          (prn e)))))
         (windows/request-frame (:chic/current-window ctx))
         (let [radius (:radius state)
               new-size (IPoint. (- (/ width scale) (* 2 radius))
                                 (- (/ height scale) (* 2 radius)))]
           (when (not= (:container-size @*state) new-size)
             (let [new-width (:width new-size)
                   new-height (:height new-size)]
               (vswap! *draw-state
                       (fn [ds]
                         (assoc ds :nodes
                                (let [vnodes (:nodes ds)
                                      t (transient vnodes)]
                                  (doseq [[id vnode] vnodes]
                                    (assoc! t id
                                            (update vnode :position
                                                    (fn [p]
                                                      (Point. (hui/clamp (:x p) 0 new-width)
                                                              (hui/clamp (:y p) 0 new-height))))))
                                  (persistent! t)))))))
           (swap! *state assoc :container-size new-size)))
       (cui/dyncomp (particle-graph**))))))))

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
        0.5 (cui/with-bounds
              :slider-bounds
              (ui/dynamic
               ctx [{:keys [slider-bounds scale]} ctx]
               (cui/on-mouse-move
                (fn [event]
                  (when (:dragging? @*vstate)
                    (let [new-pos (/ (:x (cui/component-relative-pos event (:chic.ui/mouse-win-pos event))) scale)
                          ;; diff (/ (- new-pos (:last-mouse-pos @*vstate)) (:width cs))
                          ]
                      ;; (vswap! *vstate assoc :last-mouse-pos new-pos)
                      (on-change (hui/clamp (/ (+ new-pos (:mouse-offset @*vstate))
                                               (- (:width slider-bounds) cursor-width)) 0 1)))))
                (cuilay/halign
                 value
                 (cui/clickable
                  (fn [event]
                    (when (and (not (:dragging? @*vstate))
                               (:hui.event.mouse-button/is-pressed event))
                      (vswap! *vstate assoc :dragging? true
                              :mouse-offset
                              (- (/ (:x (cui/component-relative-pos
                                         event (:chic.ui/mouse-win-pos event))) scale)))))
                  (cuilay/width
                   cursor-width
                   (cuilay/height
                    #(:width %) (oval-shape (Paint.)))))))))))))))

(defn checkbox [{:keys [checked? on-change]}]
  (cui/clickable
   (fn [event]
     (when (:hui.event.mouse-button/is-pressed event)
       (on-change (not checked?))))
   (ui/clip-rrect
    4
    (if checked?
      (ui/fill (huipaint/fill 0xFF0090FF)
               (ui.svg/make (maticons/svg-data "check" "round" "24px")))
      (ui/fill (huipaint/fill 0xFF000000)
               (cuilay/padding
                2 (ui/clip-rrect
                   2 (ui/fill (huipaint/fill 0xFFFFFFFF)
                              (ui/gap 0 0)))))))))

(defn initial-slider-state []
  (volatile! {:dragging? false}))

(defn particle-graph-with-controls [{:keys [*state] :as model}]
  ;; (def model model)
  (ui/dynamic
   ctx [{:keys [font-ui fill-text]} ctx]
   (let [aslider
         (fn [paramkeyvec {:keys [min-limit max-limit label state]}]
           (let [state (initial-slider-state)]
             [:stretch 1
              (ui/dynamic
               _ [param (get-in @*state paramkeyvec)]
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
                                 (swap! *state assoc-in paramkeyvec
                                        (if (zero? min-limit)
                                          (unchecked-add min-limit
                                                         (unchecked-multiply (float value) (- max-limit min-limit)))
                                          (* min-limit (math/pow (/ max-limit min-limit)
                                                                 (float value))))))})))))]))
         acheckbox
         (fn [paramkeyvec {:keys [label]}]
           (ui/dynamic
            _ [checked? (get-in @*state paramkeyvec)]
            (cui/dyncomp
             (cuilay/padding
              1 0 (cuilay/valign
                   0.5 (cuilay/row
                        (cuilay/width
                         18 (cuilay/height
                             18 (checkbox
                                 {:checked? checked?
                                  :on-change (fn [checked?]
                                               (swap! *state assoc-in paramkeyvec checked?))})))
                        (cuilay/padding
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
       (acheckbox [:physics-params :compression?] {:label "compression"})
       (acheckbox [:physics-params :repel-connected?] {:label "repel connected"})
       (acheckbox [:physics-params :distribute-spring?] {:label "distribute spring"})
       (acheckbox [:view-config :suppress-shortcuts?] {:label "suppress shortcuts"}))
      (cuilay/row
       (aslider [:physics-params :attraction-coeff] {:label "stiffness" :min-limit 1 :max-limit 1000
                                                     :state attraction-slider-state})
       (aslider [:physics-params :attraction-deadzone] {:label "natural length" :min-limit 01 :max-limit 1000})
       (aslider [:physics-params :repulsion-coeff] {:label "repulsion" :min-limit 100 :max-limit 100000000
                                                    :state repulsion-slider-state})
       (aslider [:physics-params :repel-range] {:label "repel-range" :min-limit 0 :max-limit 1000}))
      (cuilay/row
       (aslider [:physics-params :time-scale] {:label "speed" :min-limit 0.001 :max-limit 0.01
                                               :state speed-slider-state})
       (aslider [:physics-params :friction-coeff] {:label "friction" :min-limit 0.1 :max-limit 100
                                                   :state friction-slider-state})
       (aslider [:physics-params :sorting-buffer] {:label "sorting buffer" :min-limit 0 :max-limit 500
                                                   :state friction-slider-state}))
      (cuilay/row
       (aslider [:physics-params :min-velocity] {:label "min velocity" :min-limit 0.001 :max-limit 1})
       (aslider [:physics-params :max-velocity] {:label "max velocity" :min-limit 0 :max-limit 10}))))))

(comment
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
                      :container-size (IPoint. 0 0)
                      :radius 7
                      :physics-params {:time-scale 0.001
                                       ;; :attraction-coeff 10
                                       :attraction-coeff 20
                                       :friction-coeff 8
                                       ;; :repulsion-coeff 1000000
                                       :repulsion-coeff 2350000
                                       ;; :attraction-deadzone 200
                                       :attraction-deadzone 50
                                       :repel-range 120
                                       :min-velocity 0.001
                                       :max-velocity 1
                                       :sorting-buffer 70
                                       :compression? true
                                       :repel-connected? false
                                       :distribute-spring? true}})
        *draw-state (volatile! {:nodes (into {}
                                             (map (fn [id]
                                                    [id {:velocity (Point. 0 0)
                                                         :position (Point. 0 0)}]))
                                             (keys nodes))
                                :t (System/currentTimeMillis)})]
    ;; (def --ds *draw-state)
    {:*state *state :*draw-state *draw-state}))

(defn particle-graph [state]
  (cui/dyncomp (particle-graph-with-controls state)))
