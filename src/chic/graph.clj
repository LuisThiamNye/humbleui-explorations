(ns chic.graph
  (:require
   [chic.ui :as cui]
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
        container-size (:container-size @*state)
        time-scale 0.001 ;; ms/s work in pixels/second
        max-dt (* time-scale 100)
        min-velocity (/ 0.001 time-scale)
        max-velocity (/ 1 time-scale)
        min-accel (/ 0.000001 time-scale)
        max-accel (/ 1 time-scale)
        dist->acc-attract (fn [dist] (max 0 (* 5 (- dist 200))))
        friction-acc (fn [vel] (vscale -10 vel))
        ;; (/ 1 (math/pow (unchecked-add 100 5) 2))  ;
        ;; dist->acc-repel (fn [dist] (min 0 (* 1000000 (unchecked-subtract 9e-5 (/ 1 (math/pow (unchecked-add dist 5) 2.))))))
        dist->acc-repel (fn [dist] (min 0 (* 1000000 (unchecked-subtract 0.005 (/ 1 (unchecked-add dist 5))))))
        disp->sorting-acc (fn [disp]
                            (let [buffer 100]
                              (* 100(cond
                                      (< disp (- buffer))
                                      0
                                      (< disp buffer)
                                      (* (+ buffer disp) 2)
                                      :else
                                      (* 2 2 50)))))
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
                               disp (:y diff)]
                           (vswap! *accel
                                   #(-> %
                                        (vadd (vscale (dist->acc-repel dist) diff-unit))
                                        (cond-> connected-to-this-node?
                                          (-> (vadd (vscale (dist->acc-attract dist) diff-unit))
                                              (vadd (vscale (if receiving-connection?
                                                              (disp->sorting-acc disp)
                                                              (- (disp->sorting-acc (- disp)))) (Point. 0 1)))))))))
                       (let [accel @*accel
                             accel-mag (vabs accel)
                             accel-mag' (min max-accel accel-mag)
                             accel (if (< min-accel accel-mag')
                                     (vscale (/ accel-mag' accel-mag) accel) (Point. 0 0))
                             v2 (vadd velocity (vscale dt accel))
                             v2-mag(vabs v2)
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
            (ui/padding
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
                                                                          (int-array 2 [(unchecked-int 0xFF0047ff)
                                                                                        (unchecked-int 0xFFff9900)])))))))})))
                         (:connected-nodes node)))))
               nodes)))
            (eduction
             (map (fn [[id node]]
                    (when-not (contains? (:nodes @*draw-state) id)
                      (vswap! *draw-state assoc-in [:nodes id]
                              {:velocity (Point. 0 0)
                               :position (Point. (+ 50 (rand-int 400)) (+ 50 (rand-int 300)))}))
                    (let [{:keys [position]} (get-in @*draw-state [:nodes id])]
                      (let [{:keys [x y]} position]
                        (cuilay/halign
                         0 (cuilay/valign
                            0 (ui/padding
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
                                (ui/custom-ui
                                 (* 2 radius) (* 2 radius)
                                 {:on-paint (fn [canvas width height]
                                              (let [radius (min (/ width 2) (/ height 2))]
                                                (.drawCircle canvas radius radius radius
                                                             (doto (Paint.) (.setColor (unchecked-int 0xFF5077ff#_0xFF10b0b0))))))})))))))))
             nodes)
            (eduction
             (map (fn [[id node]]
                    (let [{:keys [position]} (get-in @*draw-state [:nodes id])
                          {:keys [x y]} position]
                      (cuilay/halign
                       0 (cuilay/valign
                          0 (ui/padding
                             #(hui/clamp (+ (* 2 radius) 2 x) 0 (:width %))
                             #(hui/clamp y 0 (:height %)) 0 0
                             (ui/height
                              (* 2 radius)
                              (ui/valign 0.5 (ui/label (:label node) font-ui fill-text)))))))))
             nodes))))))))))

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
                      :dragging-nodes #{}})
        *draw-state (volatile! {:nodes {}
                                :t (System/currentTimeMillis)})]
    (def --ds *draw-state)
    {:*state *state :*draw-state *draw-state}))

(defn particle-graph [state]
  (cui/dyncomp (particle-graph* state)))
