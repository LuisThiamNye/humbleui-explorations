(ns chic.quantum.demo
  (:require
   [clojure.core.matrix :as matrix]
   [chic.paint :as cpaint]
   [chic.util :as util]
   [chic.ui.event :as uievt]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as huipaint]
   [chic.ui.error :as cui.error]
   [chic.ui :as cui]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Paint Shader Canvas PaintMode]
   [io.github.humbleui.types IPoint Point Rect]))

(defn varg [[x y]] (cond->> (Math/acos x) (neg? y) (- (* 2 Math/PI))))

(defn oval-shape [paint]
  (cui/on-draw
   (fn [_ cs ^Canvas canvas]
     (.drawOval canvas (Rect/makeXYWH 0 0 (:width cs) (:height cs)) paint))
   (ui/gap 0 0)))

(defn slider [{:keys [value on-change value->pos]}]
  (let [cursor-width 12
        value->pos (or value->pos identity)
        *vstate (volatile! {:dragging? false})]
    (cui/dynamic
     ctx [value (hui/clamp (value->pos (get ctx value)) 0 1)]
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
               (cui/dynamic
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
                     #(:width %) (oval-shape (Paint.))))))))))))))))

(defn qstate-after-gate [circuit qid gid]
  (let [gates (get-in circuit [:qubits qid :gates])
        idx (util/index-of gates gid)]
    (get-in circuit [:qubit-states qid idx])))

(defn latest-qubit-state [circuit]
  (:state-vec circuit))

(def h-matrix (matrix/mul (/ (Math/sqrt 2))
                          [[1 1]
                           [1 -1]]))

(def z-matrix [[1 0]
               [0 -1]])

(def x-matrix [[0 1]
               [1 0]])

(def cnot-matrix [[1 0 0 0]
                  [0 1 0 0]
                  [0 0 0 1]
                  [0 0 1 0]])

(defn tensor-prod [m1 m2]
  (matrix/mul
   (into []
         (mapcat (fn [row]
                   (repeat
                    (matrix/row-count m2)
                    (into [] (mapcat #(repeat (matrix/column-count m2) %))
                          row))))
         m1)
   (into []
         cat
         (repeat
          (matrix/row-count m1)
          (mapv #(into [] cat (repeat (matrix/column-count m1) %)) m2)))))

(defn mat-for* [filler qn n mat]
  (let [r (reduce tensor-prod
                  (conj (vec (repeat qn filler))
                        mat))
        r (reduce tensor-prod
                  r (repeat (- n qn (int (/ (matrix/column-count mat) 2)))
                            filler))]
    r))

(defn mat-for [qn n mat]
  (mat-for* (matrix/identity-matrix 2) qn n mat))

(defn prob-for [qn v]
  (let [run-length (Math/round (float (/ (count v) (Math/pow 2 (inc qn)))))
        sqtotal (reduce + (map #(* % %) v))
        ones (eduction
              (mapcat
               (fn [i]
                 (let [i (* 2 run-length i)]
                   (subvec v (+ run-length i) (+ (* 2 run-length) i)))))
              (range (inc qn)))
        sqones (reduce + (map #(* % %) ones))]
    (float (/ sqones
              sqtotal))))

(comment
  (prob-for 0 [0 1 0 2])
  (prob-for 1 [0.71 0 0.71 0 0 0 0 0])
  (mat-for 3 3 [[0 1]
                [1 0]])
  (tensor-prod cnot-matrix [[1 0]
                            [0 1]])
  (tensor-prod [[0 1]
                [1 0]]
               [[1 0]
                [0 1]])
  (matrix/mmul (mat-for 1 3 cnot-matrix)
               (mat-for 1 3 h-matrix)
               [1 0 0 0 0 0 0 0])
  (matrix/mmul (mat-for 0 3 h-matrix)
               [0.5 0 0 0.5 0 0.5 0.5 0])
  (matrix/mmul (mat-for 0 1 h-matrix)
               [0.71 0.71])
  (doseq [l(mat-for 0 3 h-matrix)]
    (prn l))
  #!
  )

(defn compute-gate-results [state qid gate]
  (if (:disabled? gate)
    {:state state :qubits [qid]}
    (case (:gate-type gate)
     :H {:state (matrix/mmul (mat-for qid 3 h-matrix) state)
         :qubits [qid]}
     :Z {:state (matrix/mmul (mat-for qid 3 z-matrix) state)
         :qubits [qid]}
     :X {:state (matrix/mmul (mat-for qid 3 x-matrix) state)
         :qubits [qid]}
     :I {:state state :qubits [qid]}
     :CNOT (let [cqb (:control-qubit gate)
                 tqb (first (:target-qubits gate))]
             {:state (matrix/mmul (mat-for cqb 3 cnot-matrix) state)
              :qubits [cqb tqb]}))))

(defn apply-gate [circuit qid gate]
  (let [latest-state (latest-qubit-state circuit)
        {:keys [state qubits]} (compute-gate-results latest-state qid gate)]
    (reduce
     (fn [circuit qid']
       (update-in circuit [:qubit-states qid']
                  (fnil conj []) {:state-vec state
                                  :probability (prob-for qid' state)
                                  :last-state-vec latest-state}))
     (assoc circuit :state-vec state)
     qubits)))

(defn compute-states [circuit]
  (let [order (:gate-order circuit)]
    (reduce (fn [circuit [qid gid]]
              (apply-gate circuit qid (get-in circuit [:gates gid])))
            (assoc circuit
                   :qubit-states {}
                   :state-vec
                   (into []
                         cat
                         (reduce (fn [sv [_qid {:keys [value]}]]
                                   (let [value (mapv vector value)]
                                     (if sv
                                       (tensor-prod sv value)
                                       value)))
                                 nil
                                 (sort-by key (:qubits circuit)))))
            order)))

(def *circuit
  (atom
   (compute-states
    {:gates
     {2 {:gate-type :H}
      3 {:gate-type :H}
      0 {:gate-type :CNOT
         :control-qubit 0
         :target-qubits #{1}}
      1 {:gate-type :CNOT
         :control-qubit 1
         :target-qubits #{2}}
      4 {:gate-type :Z}
      5 {:gate-type :X}}
     :gate-order [[1 3] [1 1] [0 0] [0 2] [2 4] [2 5]]
     :qubits
     {0 {:gates [0 2]
         :value [1 0]}
      1 {:gates [3 1 0]
         :value [1 0]}
      2 {:gates [1 4 5]
         :value [1 0]}}
     :qubit-states {}
     :state-vec nil})))

(comment
  (compute-states @*circuit)
  (qstate-after-gate @*circuit 2 4)
  (tap> (:qubit-states @*circuit))
  (matrix/mmul cnot-matrix [0.7 0.7 1 0])
  (matrix/mmul h-matrix [1 0])
  #_(reduce
     (fn [circuit qubit]
       (reduce
        (fn [circuit gates]
          (reduce (fn [circuit gate-id]
                    (let [gate (get-in circuit [:gates gate-id])]
                      (assoc-in circuit [:gates gate-id :xpos]
                                (fnil + 0))))
                  circuit
                  gates))
        circuit
        (:gates qubit)))
     circuit
     (:qubits circuit))

  #!
  )

(defn ui-gate-box [child]
  (cui/dynamic
    ctx [disabled? (:disabled? ctx)]
    (cuilay/valign
    0.5 (cuilay/halign
         0.5 (cuilay/padding
              1 (ui/fill
                 (huipaint/stroke 0xFF000000 2)
                 (ui/fill
                  (huipaint/fill (if disabled?
                                   0xFFa0a0a0
                                   0xFFFFFFFF))
                  child)))))))

(defn ui-letter-gate [letter]
  (cui/dynamic
    ctx [{:keys [font-ui fill-text]} ctx]
    (ui-gate-box
     (cuilay/padding
      10 (ui/label (str letter) font-ui fill-text)))))

(defn ui-h-gate []
  (ui-letter-gate "H"))

(defn ui-z-gate []
  (ui-letter-gate "Z"))

(defn ui-x-gate []
  (ui-letter-gate "X"))

(defn indicator [{:keys [angle]}]
  {:pre [(not (Double/isNaN angle))]}
  (let [red (huipaint/fill 0xFFFF0000)
        white (huipaint/fill 0xFFFFFFFF)
        grey (huipaint/fill 0xFFc0c0c0)]
    (cui/on-draw
     (fn [_ {:keys [width height]} ^Canvas canvas]
       (let [r (/ width 2)
             x (+ r (* r (Math/cos angle)))
             y (- r (* r (Math/sin angle)))]
         (.drawOval canvas (Rect/makeXYWH 0 0 width height) white)
         (.drawLine canvas r 0 r height grey)
         (.drawLine canvas 0 r width r grey)
         (.drawLine canvas r r x y red)))
     (cuilay/padding
      1 (oval-shape (huipaint/stroke 0xFF101010 1))))))

(defn half-indicator [{:keys [angle]}]
  {:pre [(not (Double/isNaN angle))]}
  (let [red (huipaint/fill 0xFFFF0000)
        white (huipaint/fill 0xFFFFFFFF)
        grey (huipaint/fill 0xFFc0c0c0)]
    (ui/clip
     (cui/on-draw
      (fn [_ {:keys [width height]} ^Canvas canvas]
        (let [r (/ width 2)
              x (+ r (* r (Math/cos angle)))
              y (- r (* r (Math/sin angle)))]
          (.drawOval canvas (Rect/makeXYWH 0 0 width (* 2 height)) white)
          (.drawOval canvas (Rect/makeXYWH 0.5 0.5 (dec width) (dec (* 2 height)))
                     (huipaint/stroke 0xFF101010 1))
          ;; guides
          (.drawLine canvas r 0 r height grey)
          (.drawLine canvas 0 height width height grey)
          ;; main
          (.drawLine canvas r (dec height) x (dec y) red)))
      (cuilay/padding
       1 (ui/gap 0 0))))))

(defn basic-view []
  (cui/dynamic
   ctx [{:keys [fill-text font-ui font-code]} ctx]
   (let [row-height 60
         gate-width 100
         ui-gate-gap (ui/gap gate-width 0)
         black (huipaint/fill 0xFF000000)
         black-stroke (huipaint/stroke 0xFF000000 2)
         gate-indicator (fn [qid gid]
                          (cui/dynamic
                           ctx [{:keys [probability]} (qstate-after-gate @*circuit qid gid)]
                           (cuilay/halign
                            0.5
                            (cuilay/valign
                             0.5 (cuilay/stack
                                  (ui/gap 30 15)
                                  (half-indicator {:angle (* Math/PI probability)}))))))
         ui-box-gate (fn [qid gid ui]
                       (cuilay/stack
                        (ui/gap gate-width 0)
                        ui
                        (cuilay/padding
                         (/ gate-width 1.5) 0 0 0
                         (gate-indicator qid gid))))
         ui-toggle-gate (fn [qid gid]
                          (cui/updating-ctx
                           (fn [ctx]
                             (assoc ctx :disabled?
                                    (get-in @*circuit [:gates gid :disabled?])))
                           (cui/clickable
                            (uievt/on-primary-down
                             (fn [_]
                               (swap! *circuit update-in [:gates gid :disabled?] not)
                               (swap! *circuit compute-states)))
                            (ui-box-gate qid gid
                                         (case (get-in @*circuit [:gates gid :gate-type])
                                           :Z (ui-z-gate)
                                           :X (ui-x-gate)
                                           :H (ui-h-gate))))))
         ;; ui-h-gate (fn [qid gid]
         ;;             (ui-box-gate qid gid (ui-h-gate)))
         ui-gate-control (fn [qid gid]
                           (cuilay/stack
                            (ui/gap gate-width 0)
                            (cuilay/valign
                             0.5 (cuilay/halign
                                  0.5 (cuilay/stack
                                       (ui/gap 10 10)
                                       (oval-shape fill-text))))
                            (cuilay/padding
                             (/ gate-width 2) 0 0 0
                             (gate-indicator qid gid))))
         ui-gate-target (fn [qid gid]
                          (cuilay/stack
                           (ui/gap gate-width 0)
                           (cuilay/valign
                            0.5 (cuilay/halign
                                 0.5
                                 (cui/on-draw
                                  (fn [_ {:keys [width height]} ^Canvas canvas]
                                    (let [r (/ width 2)]
                                      (.drawLine canvas r 0 r height black-stroke)
                                      (.drawLine canvas 0 r width r black-stroke)
                                      (.drawOval canvas (Rect/makeXYWH
                                                         0.5 0.5 (- width 1) (- height 1))
                                                 black-stroke)))
                                  (cuilay/stack
                                   (ui/gap 22 22)))))
                           (cuilay/padding
                            (/ gate-width 2) 0 0 0
                            (gate-indicator qid gid))))
         ui-vline (fn [row col length]
                    (cuilay/translate
                     (+ 10 -1 (* (+ 0.5 col) gate-width))
                     (* (+ 0.5 row) row-height)
                     (cuilay/halign
                      0 (cuilay/valign
                         0 (ui/fill black (ui/gap 2 (* length row-height)))))))]
     (cui/updating-ctx
      (fn [ctx]
        (assoc ctx :alice-state (get-in @*circuit [:qubits 0 :value])))
      (cuilay/column
       (cuilay/row
        (cuilay/column
         (for [[id qubit] (sort-by key (:qubits @*circuit))]
           (cuilay/height
            row-height
            (cuilay/valign
             0.5 (cuilay/padding
                  4 0
                  (ui/label (if (= 0 id)
                              "|ψ⟩"
                              "|0⟩")
                            font-ui fill-text))))))
        (cuilay/stack
         (cuilay/column
          (for [ui-gates [[ui-gate-gap ui-gate-gap (ui-gate-control 0 0) (ui-toggle-gate 0 2)]
                          [(ui-toggle-gate 1 3) (ui-gate-control 1 1) (ui-gate-target 1 0)]
                          [ui-gate-gap (ui-gate-target 2 1) ui-gate-gap ui-gate-gap
                           (ui-toggle-gate 2 4) (ui-toggle-gate 2 5)]]]
            (cuilay/height
             row-height
             (cuilay/stack
              (cuilay/valign
               0.5 (ui/fill
                    (huipaint/fill 0xFF000000)
                    (ui/gap 0 2)))
              (cuilay/row (ui/gap 10 0)
                          (eduction ui-gates)
                          (ui/gap 5 0))))))
         (ui-vline 1 1 1)
         (ui-vline 0 2 1))
        (cuilay/column
         (cuilay/height
          row-height
          (cuilay/valign
           0.5 (cuilay/padding
                4 0
                (ui/label "z" font-ui fill-text))))
         (cuilay/height
          row-height
          (cuilay/valign
           0.5 (cuilay/padding
                4 0
                (ui/label "x" font-ui fill-text))))))
       (cuilay/padding
        5 (cuilay/row
           (cuilay/valign
            0.5 (ui/label "|ψ⟩" font-ui fill-text))
           (ui/gap 5 0)
           (cuilay/valign
            0.5 (cui/dynamic
                 ctx [alice-state (:alice-state ctx)]
                 (cuilay/width
                  30 (cuilay/height
                      30 (cui/dyncomp (indicator {:angle (varg alice-state)}))))
                 #_(ui/label (format "%.3f" (float (varg alice-state))) font-code fill-text)))
           (ui/gap 5 0)
           [:stretch 1
            (cui/dyncomp
             (slider {:on-change (fn [v]
                                   (let [v (* 2 Math/PI v)]
                                     (swap! *circuit
                                            (fn [circuit]
                                              (compute-states
                                               (-> circuit
                                                   (assoc-in [:qubits 0 :value]
                                                             [(Math/cos v) (Math/sin v)])))))))
                      :value->pos (comp (fn [v]
                                          (/ v 2 Math/PI)) varg)
                      :value :alice-state}))]))
       (cuilay/padding
        5 (cui/dynamic
           _ [state-vec (:state-vec @*circuit)]
           (cuilay/row
            (for [i (range (count state-vec))]
              (let [amp (nth state-vec i)
                    prob (float (* amp amp))
                    bstr (Long/toBinaryString i)]
                (cuilay/padding
                 8 (cuilay/column
                    (ui/label (str (apply str (repeat (- 3 (count bstr)) "0")) bstr)
                              font-code fill-text)
                    (ui/gap 0 8)
                    (ui/label (format "%.3f" prob)
                              font-code fill-text)
                    (ui/gap 0 8)
                    (cuilay/height
                     150 (cuilay/column
                          (ui/fill
                           (huipaint/fill (cpaint/okhsv 0.6 0.2 1))
                           (cuilay/height
                            #(* prob (:height %))
                            (ui/gap 0 0)))
                          [:stretch 1(ui/gap 0 0)]))))))))))))))
