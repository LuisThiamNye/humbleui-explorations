(ns chic.bifurcan
  (:require
   [clojure.walk :as walk]
   [chic.util :as util]
   [better-cond.core :refer [cond] :rename {cond cond+}]
   [clj-commons.primitive-math :as prim]
   [potemkin :refer [def-map-type unify-gensyms deftype+]])
  (:import
   (io.lacuna.bifurcan Map)))

(declare ->WrappedMapTransient)

(def-map-type WrappedMap [^Map m mta]
  (get [_ k default-value]
       (.get m k default-value))
  (assoc [_ k v]
         (WrappedMap. (.put m k v) mta))
  (dissoc [_ k]
          (WrappedMap. (.remove m k) mta))
  (keys [_]
        (.keys m))
  (meta [_]
        mta)
  (with-meta [_ mta]
    (WrappedMap. m mta))
  clojure.lang.IEditableCollection
  (asTransient [_] (->WrappedMapTransient (.linear m))))

(deftype+ WrappedMapTransient [^Map m]
  potemkin.collections/PotemkinFn
  (invoke [_ k] (.get m k nil))
  (invoke [_ k not-found] (.get m k not-found))

  clojure.lang.ITransientMap
  (conj [self x]
        (cond+
         (instance? java.util.Map$Entry x)
         (let [e ^java.util.Map$Entry x]
           (.put m (.getKey e) (.getValue e)))
         (instance? clojure.lang.IPersistentVector x)
         (let [v ^clojure.lang.IPersistentVector x]
           (when-not (prim/== (util/compile (prim/int 2)) (.count v))
             (throw (IllegalArgumentException. "Vector arg to map conj must be a pair")))
           (.put m (.nth v 0) (.nth v 1)))

         (loop [es (clojure.lang.RT/seq x)]
           (when-not (clojure.lang.Util/identical es nil)
             (let [e ^java.util.Map$Entry (.first es)]
               (.put m (.getKey e) (.getValue e))
               (recur (.next es))))))
        self)
  (assoc [self k v]
         (.put m k v)
         self)
  (without [self k]
           (.remove m k)
           self)
  (valAt [_ k] (.get m k nil))
  (valAt [_ k not-found] (.get m k not-found))
  (count [_] (.size m))
  (persistent [_]
              (->WrappedMap (.forked m) nil))

  clojure.lang.ITransientAssociative2
  (entryAt [_ k]
           (let [v (.get m k ::not-found)]
             (when-not (clojure.lang.Util/identical ::not-found v)
               (clojure.lang.MapEntry/create k v))))
  (containsKey [_ k] (.contains m k)))

(defmacro hmap [& kvs]
  (assert (or (== 1 (count kvs))
              (even? (count kvs))))
  `(->WrappedMap
    ~(if (seq kvs)
       (let [entries (if (== 1 (count kvs))
                       (seq (first kvs))
                       (partition 2 kvs))]
         (unify-gensyms
          `(let [m## (.linear (Map.))]
             ~@(map (fn [[k v]]
                      `(.put m## ~k ~v))
                    entries)
             (.forked m##))))
       `(Map.)) nil))

(defmacro hset [& items])
(defmacro vect [& items])

(defmacro with-colls [types & body]
  (let [conv-map (if (some #{:map} types)
                   (fn [form] `(hmap ~form))
                   identity)
        conv-set (if (some #{:set} types)
                   (fn [form] `(hset ~form))
                   identity)
        conv-vec (if (some #{:vec} types)
                   (fn [form] `(vect ~form))
                   identity)]
    `(do
       ~@(walk/postwalk
          (fn [form]
            (cond+
               (map? form)
               (conv-map form)
               (vector? form)
               (conv-vec form)
               (set? form)
               (conv-set form)
               form))
          body))))

(comment
  (type (keys (hmap {4 4})))
  (let [m (hmap :x 4 :some 435 "some" :butter)
        t (transient m)
        t (dissoc! t :x)]
    [m (persistent! t)])
  (type (first (seq (hmap 4 4))))
  (tap> (hmap :x :y ))

  #!
  )
