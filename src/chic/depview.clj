(ns chic.depview
  (:require
   [chic.graph :as graph]
   [chic.ui :as cui]
   [chic.ui.layout :as cuilay]
   [clj-kondo.core :as kondo]
   [clojure-lsp.api :as lsp]
   [clojure.java.io :as io]
   [clojure.math :as math]
   [clojure.string :as str]
   [io.github.humbleui.ui :as ui]
   [tech.droit.fset :as fset])
  (:import
   [io.github.humbleui.skija Paint]
   [io.github.humbleui.types IPoint IRect]))

(defn namespace->file [ns]
  (let [path (-> (str ns)
                 (str/replace #"\." "/")
                 (str/replace #"-" "_"))]
    (str "src/" path ".clj")))

(def ^:dynamic *seen-nodes* [])

(defn bump-popularity [db id]
  (when (some #{id} *seen-nodes*)
    (throw (ex-info "Circular dependency" {:seen-nodes *seen-nodes*
                                           :current-node id})))
  (binding [*seen-nodes* (conj *seen-nodes* id)]
    (let [db (update-in db [id :popularity] (fnil inc 0))]
      (reduce (fn [db nid]
                (bump-popularity db nid))
              db
              (:nodes (get db id))))))

(defn get-connections [ana]
  (reduce (fn [acc {:keys [from to]}]
            (update acc (str from) update :nodes (fnil conj []) (str to)))
          {}
          (:namespace-usages ana)))

#_(defn compute-popularities [db]
    (reduce))


(defn circular-graph [{:keys [nodes]}]
  (let [*state (atom {:nodes nodes})
        *draw-state (volatile! {})
        line-paint (doto (Paint.) (.setStrokeWidth 2.) (.setColor (unchecked-int 0xFF30c0c0)))]
    (ui/dynamic
      ctx [{:keys [nodes]} @*state
           font-ui (:font-ui ctx)
           fill-text (:fill-text ctx)]
      (let [children-ctors
            (for [[id node] (sort-by (comp :popularity val) nodes)]
              (let [circle (ui/custom-ui
                            20 20
                            {:on-paint (fn [canvas width height]
                                         (let [radius (min (/ width 2) (/ height 2))]
                                           (.drawCircle canvas radius radius radius
                                                        (doto (Paint.) (.setColor (unchecked-int 0xFF10b0b0))))))})
                    label (ui/padding
                           0 4 0 3(ui/label id font-ui fill-text))]
                (fn [ctx]
                  (let [circle-size (cui/measure-child-unbounded circle ctx)
                        stack (cuilay/->Stack [(cuilay/halign 0.5 circle)
                                               (cuilay/column (ui/gap 0 (:height circle-size))
                                                              label)])
                        stack-size (cui/measure-child-unbounded stack ctx)]
                    {:centre-pos (IPoint. (/ (:width stack-size) 2)
                                          (/ (:height circle-size) 2))
                     :node node
                     :id id
                     :size stack-size
                     :layers (:children stack)}))))
            calc-node-rect (fn [graph-centre radius i {:keys [centre-pos size]}]
                             (let [angle (* 2 math/PI (/ i (count nodes)))
                                   offsetx (* radius (math/cos angle))
                                   offsety (* radius (math/sin angle))
                                   centre-on-area (IPoint.
                                                   (+ offsetx (:x graph-centre))
                                                   (+ offsety (:y graph-centre)))]
                               (IRect/makeXYWH
                                (- (:x centre-on-area) (:x centre-pos))
                                (- (:y centre-on-area) (:y centre-pos))
                                (:width size) (:height size))))]
        (cuilay/size-dependent
         (fn [cs]
           (ui/contextual
            (fn [ctx]
              (let [children (mapv #(% ctx) children-ctors)
                    radius (/ (min (- (:width cs) (reduce max 0 (map (comp :width :size) children)))
                                   (- (:height cs) (* 2(reduce max 0 (map (comp :height :size) children)))))
                              2)
                    graph-centre (IPoint. (/ (:width cs) 2)
                                          (/ (:height cs) 2))
                    children (vec (map-indexed (fn [i child]
                                                 (assoc child :rect (calc-node-rect graph-centre radius i child)))
                                               children))
                    layer-map (reduce (fn [acc i]
                                        (let [{:keys [layers rect]} (nth children i)]
                                          (reduce (fn [acc li]
                                                    (update acc li (fnil conj []) {:layer (nth layers li)
                                                                                   :rect rect}))
                                                  acc
                                                  (range (count layers)))))
                                      {} (range (count children)))
                    id->child (into {} (map (fn [child] [(:id child) child])) children)]
                (cuilay/stack
                 (for [ichild (range (count children))]
                   (let[{:keys [rect centre-pos] :as child} (nth children ichild)]
                     (for [other-node-id (:nodes (:node child))]
                       (let [other-child (id->child other-node-id)
                             other-rect (:rect other-child)]
                         (when other-child
                           (ui/custom-ui
                            (:width cs) (:height cs)
                            {:on-paint (fn [canvas _width _height]
                                         (.drawLine canvas
                                                    (+ (:x rect) (:x centre-pos))
                                                    (+ (:y rect) (:y centre-pos))
                                                    (+ (:x other-rect) (:x (:centre-pos other-child)))
                                                    (+ (:y other-rect) (:y (:centre-pos other-child)))
                                                    line-paint))}))))))
                 (for [layers (map val (sort-by key layer-map))]
                   (for [{:keys[layer rect]} layers]
                     (cuilay/halign
                      0 (cuilay/valign
                         0 (cuilay/padding
                            (:x rect) (:y rect) 0 0
                            (ui/width (:width rect)
                                      (ui/height (:height rect)layer)))))))))))))))))

(defn filter-nodes [pred nodes]
  (let [good-ids (into #{}
                       (comp (filter (fn [[k m]] (pred m)))
                             (map key))
                       nodes)]
    (into {}
         (comp
          (filter (comp good-ids key))
          (map (fn [[k m]]
                 [k (update m :connected-nodes fset/intersection good-ids)])))
         nodes)))


(comment
  (lsp/analyze-project-and-deps! {:file (io/file ".")})
  (first (vals (:diagnostics (lsp/diagnostics {:namespace '[chic.cljbwr]

                                               :output {:canonical-paths true}}))))
  (def ana (:analysis
            (kondo/run! {:lint [(io/file ".")]
                         :cache true
                         :config {:output {:analysis {:arglists false
                                                      :locals false
                                                      :keywords false
                                                      :protocol-impls false
                                                      :java-class-definitions false}
                                           :canonical-paths true}}})))

  (defn ana->nodes []
    #_(let [connections (get-connections ana)]
      (let [all-ids (into #{} (map hash) (keys connections))]
        (into {}
              (map (fn [[ns {:keys [nodes]}]]
                     [(hash ns) {:connected-nodes
                                 (into #{} (comp (map hash) (filter all-ids)) nodes)
                                 :label ns}]))
              connections)))
    (let [connections (get-connections ana)]
      (let [all-ids (into #{} (map hash) (keys connections))]
        (reduce (fn [db [ns {:keys [nodes]}]]
                  (let [db (reduce (fn [db ns]
                                     (assert (some? ns))
                                     (if (contains? db (hash ns))
                                       db
                                       (assoc db (hash ns) {:connected-nodes #{}
                                                            :label ns})))
                                   db
                                   (conj nodes ns))]
                    (assoc-in db [(hash ns) :connected-nodes]
                              (into #{} (map hash) nodes))))
                {}
                connections))))
  (def graph-nodes
    (filter-nodes
     #(str/starts-with? (:label % "") "chic")
     (ana->nodes)))
  (def graph-nodes (ana->nodes))
  (map :label (vals graph-nodes))

  (map (fn [[n m]]
         [n (:popularity m)])
       (sort-by (comp :popularity val) (bump-popularity connections "chic.main")))
  (get connections "chic.main")
  *compile-path*

  (first (filter #(= (str (:to %)) "chic.main") (filter #(= (str (:from %)) "chic.main") (:namespace-usages ana))))

  (def circle-nodes
    (let [pred #(str/starts-with? (:label % "") "chic")]
      (let [connections (bump-popularity (get-connections ana) "chic.main")]
          (let [all-ids (into #{} (keys connections))]
            (into {}
                  (map (fn [[ns {:keys [nodes]}]]
                         [ns {:nodes
                                     (into #{} (comp (filter all-ids)) nodes)
                                     :label ns}]))
                  connections)))))
  #!
  )
(defonce graph-nodes {})
(defonce circle-nodes {})

(defn basic-view []
  (let [graph-state (graph/particle-graph-new-state graph-nodes)]
    (cui/dyncomp
     #_(circular-graph {:nodes circle-nodes})
    (graph/particle-graph graph-state))))

(comment

  #!
  )
