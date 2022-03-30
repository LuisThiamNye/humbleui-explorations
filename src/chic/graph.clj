(ns chic.graph
  (:require
   [babashka.fs :as fs]
   [clojure.math :as math]
   [clojure-lsp.api :as lsp]
   [clj-kondo.core :as kondo]
   [chic.ui.text-input :as text-input]
   [taoensso.encore :as enc]
   [chic.ui :as cui]
   [chic.ui.error :as cui.error]
   [chic.ui.layout :as cuilay]
   [clojure.java.io :as io]
   [chic.ui.svg :as ui.svg]
   [chic.ui.icons.material :as maticons]
   [chic.debug :as debug]
   [chic.focus :as focus]
   [chic.clj.source :as source]
   [clojure.string :as str]
   [clojure.repl :as repl]
   [nrepl.server :as nrepl-server]
   [chic.text-editor :as text-editor]
   [chic.text-editor.core :as text-editor.core]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as paint]
   [io.github.humbleui.profile :as profile]
   [cider.nrepl :refer [cider-nrepl-handler]]
   [chic.windows :as windows]
   [io.github.humbleui.window :as window]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventMouseScroll
    EventKey Window EventWindowFocusOut MouseButton]
   [io.github.humbleui.skija Canvas FontMgr FontStyle Typeface Font Paint PaintMode]
   [io.github.humbleui.types IPoint IRect Point]))

(defn vadd [a b] (Point. (unchecked-add (:x a) (:x b)) (unchecked-add (:y a) (:y b))))
(defn vsub [a b] (Point. (unchecked-subtract (:x a) (:x b)) (unchecked-subtract (:y a) (:y b))))

(defn particle-graph-do-physics [nodes *state *draw-state]
  (let [vscale (fn [s v] (Point. (unchecked-multiply s (:x v)) (unchecked-multiply s (:y v))))
        vdot (fn [a b] (unchecked-add (unchecked-multiply (:x a) (:x b)) (unchecked-multiply (:y a) (:y b))))
        vabs (fn [v] (math/sqrt (vdot v v)))
        time-scale 0.001 ;; work in pixels/second
        max-dt (* time-scale 100)
        min-velocity (/ 0.001 time-scale)
        min-accel (/ 0.000001 time-scale)
        dist->acc-attract (fn [dist] (max 0 (* 0.0000005 (- dist 200))))
        dist->acc-attract2 (fn [dist] (* 0.0000001 (+ dist 20)))
        damping-acc (fn [vel] (vscale -0.1 vel))
        ;; (unchecked-subtract 0.003 (float (/ 1 (unchecked-add 50 5))))
        dist->acc-repel (fn [dist] (min 0 (* 0.002 (unchecked-subtract 0.01 (/ 1 (unchecked-add dist 5))))))
        dist->acc-repel2 (fn [dist] (min 0 (* 0.002 (unchecked-subtract 0.01 (/ 1 (unchecked-add dist 5))))))
        t2 (System/currentTimeMillis)]
    (vswap! *draw-state
            (fn [{:keys [t] vnodes :nodes :as ds}]
              (let [dt (min max-dt (* time-scale (unchecked-subtract t2 t)))]
                (reduce
                 (fn [ds [id {:keys [position velocity] :as vnode}]]
                   (if (and position (not (contains? (:dragging-nodes @*state) id)))
                     (let [connected-nodes (:connected-nodes (get nodes id))
                           *accel (volatile! (damping-acc velocity))]
                       (doseq [kv (dissoc vnodes id)]
                         (let [other-vnode-id (key kv)
                               other-vnode (val kv)
                               diff (vsub (:position other-vnode) position)
                               dist (vabs diff)
                               diff (if (== 0 dist)
                                      (IPoint. (+ 0.5 (rand)) (+ 0.5 (rand)))
                                      diff)
                               diffy (.withX diff 0)
                               diffx (.withY diff 0)
                               dist (if (== 0 dist) (max 1 (vabs diff)) dist)
                               distx (abs (:x diffx))
                               disty (abs (:y diffy))]
                           (vswap! *accel
                                   #(-> %
                                        (vadd (vscale (/ (dist->acc-repel dist) (max 1 (min dist 100))) diff))
                                        (cond-> (or (contains? connected-nodes other-vnode-id)
                                                    (contains? (:connected-nodes (get nodes other-vnode-id)) id))
                                          (vadd (vscale (/ (dist->acc-attract dist) (max 1 dist) #_(count connected-nodes)) diff)))
                                        (cond->
                                         (contains? connected-nodes other-vnode-id)
                                          (vadd (if (pos? (:y diff))
                                                  (vscale (+ (/ (dist->acc-repel2 disty) (max 1 disty) #_(count connected-nodes))) diffy)
                                                  (vscale (+ (/ (dist->acc-attract2 disty) (max 1 disty) #_(count connected-nodes))) diffy))))
                                        (cond-> (contains? (:connected-nodes (get nodes other-vnode-id)) id)
                                          (vadd (if (pos? (:y diff))
                                                  (vscale (+ (/ (dist->acc-attract2 disty) (max 1 disty) #_(count (:connected-nodes (get nodes other-vnode-id))))) diffy)
                                                  (vscale (+ (/ (dist->acc-repel2 disty) (max 1 disty) #_(count (:connected-nodes (get nodes other-vnode-id))))) diffy))))))))
                       (let [accel @*accel
                             accel (if (< min-accel (vabs accel)) accel (Point. 0 0))
                             v2 (vadd velocity (vscale dt accel))
                             v2 (if (< min-velocity (vabs v2)) v2 (Point. 0 0))]
                         (assoc-in
                          ds [:nodes id]
                          (-> vnode
                              (assoc :velocity v2)
                              (assoc :position (vadd position (vscale dt v2)))))))
                     ds))
                 (assoc ds :t t2)
                 vnodes))))))

(defn particle-graph* [{:keys [*state *draw-state]}]
  (let [radius 7
        line-paint (doto (Paint.) (.setStrokeWidth 1.5) (.setColor (unchecked-int 0xFF30c0c0)))]
    (cui/clickable
     (fn [event]
       (when (and (not (:hui.event.mouse-button/is-pressed event))
                  (seq (:dragging-nodes @*state)))
         (swap! *state assoc :dragging-nodes #{})))
     (cui/on-mouse-move
      (fn [event]
        (when-let [ids (seq (:dragging-nodes @*state))]
          (vswap! *draw-state (fn [ds]
                                (reduce (fn [ds id]
                                          (assoc-in ds [:nodes id :position]
                                                    (vadd (:mouse-drag-offset @*state)
                                                          (:chic.ui/mouse-win-pos event))))
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
                                             (.drawLine canvas (:x pos1) (:y pos1)
                                                        (:x pos2) (:y pos2) line-paint)))})))
                      (:connected-nodes node)))))
            nodes)
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
                              #(hui/clamp (unchecked-subtract x radius) 0 (:width %))
                              #(hui/clamp (unchecked-subtract y radius) 0 (:height %)) 0 0
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
                                                            (doto (Paint.) (.setColor (unchecked-int 0xFF10b0b0))))))})))))))))
            nodes)
           (eduction
            (map (fn [[id node]]
                   (let [{:keys [position]} (get-in @*draw-state [:nodes id])
                         {:keys [x y]} position]
                     (cuilay/halign
                      0 (cuilay/valign
                         0 (ui/padding
                            #(hui/clamp (+ (* 2 radius) 2 (unchecked-subtract x radius)) 0 (:width %))
                            #(hui/clamp (unchecked-subtract y radius) 0 (:height %)) 0 0
                            (ui/height
                             (* 2 radius)
                             (ui/valign 0.5 (ui/label (:label node) font-ui fill-text)))))))))
            nodes)))))))))

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
