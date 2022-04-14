(ns chic.util
  (:require
   [potemkin :refer [unify-gensyms doit]]
   [riddley.walk :as rwalk]))

(defn url->bytes [url]
  (with-open [^java.io.InputStream input-stream (.openStream (java.net.URL. url))]
    (loop [bs []]
      (let [b (.read input-stream)]
        (if (== -1 b)
          (byte-array bs)
          (recur (conj bs b)))))))

(defn assoc-if-not= [m k v]
  (if (= (get m k) v)
    m
    (assoc m k v)))

(defmacro compile [& body]
  (eval `(do ~@body)))

(definterface IMutable
  (reset [v]))

(deftype Mutable [^:unsynchronized-mutable value]
  chic.util.IMutable
  (reset [_ v] (set! value v))
  clojure.lang.IDeref
  (deref [_] value))

(defn mutable! [value]
  (Mutable. value))

(definline mreset! [mut v]
  `(.reset ~(vary-meta mut assoc :tag `IMutable) ~v))

(defn index-of [coll x]
  (.indexOf ^clojure.lang.APersistentVector coll x))

(def *template-var->generated-var
  (atom {}))

(defn generate-var-for-template [ctor t namsp sym]
  (assert (symbol? sym))
  (assert (some? t))
  (assert (map? t))
  (let [template (ctor t)
        params (:params template)
        _ (assert (vector? params))
        code `(fn ~sym ~params
                ~@(:body template))
        vr (intern namsp sym (eval code))]
    (alter-meta! vr (fn [_] (assoc (meta sym)
                                   ::template-ctor ctor
                                   ::code code)))
    vr))

(defn regenerate-vars-for-template [tvar]
  (let [namsp (find-ns (symbol (namespace (symbol tvar))))]
    (doit [gvar (get @*template-var->generated-var tvar)]
      (when-some [ctor (::template-ctor (meta gvar))]
        (let [sym (symbol (name (symbol gvar)))]
          (try (generate-var-for-template ctor @tvar namsp sym)
              (catch Throwable e
                (println "error happened generating" gvar "\n"
                         (str e)))))))))

(defmacro deftemplate [sym params & body]
  `(do (def ~sym
         {:sym ~sym
          :params (quote ~params)
          :body (quote [~@body])})
       ~(when (some? (get @*template-var->generated-var (resolve sym)))
          `(regenerate-vars-for-template (var ~sym)))
       (var ~sym)))

(defmacro defgenerated [sym params tsym ctor']
  (let [ctor (eval ctor')
        tvar (resolve tsym)
        template (ctor @tvar)
        gvar (generate-var-for-template ctor @tvar *ns* sym)]
    (swap! *template-var->generated-var
           update tvar (fnil conj #{}) gvar)
    (assert (= (count params) (count (:params template))))
    gvar))

(defn sub-template-args [template & specs]
  (reduce
   (fn [template [sym typ f]]
     (when-not (some #{sym} (:params template))
       (throw (AssertionError. (str "Could not find " sym " in params of template " (:sym template) \newline
                                    (:params template "(it's nil)")))))
     (case typ
       :inline-fn
       (assoc template
              :body (rwalk/walk-exprs
                     (fn [expr]
                       (and (seq? expr) (= sym (first expr))))
                     (fn [[_ & args]]
                       (apply f args))
                     (:body template))
              :params (into [] (remove #{sym}) (:params template)))))
   template
   specs))

(defn constant-literal? [thing]
  (or (keyword? thing)
      (symbol? thing)
      (number? thing)
      (string? thing)
      (instance? java.util.regex.Pattern thing)))

(defmacro let-macro-syms [bindings & body]
  (unify-gensyms
   `(let
     ~(into `[symbinds## (java.util.ArrayList.)]
            cat
            [(eduction
              (partition-all 2)
              (mapcat
               (fn [[sym v]]
                 [sym v
                  sym `(if (constant-literal? ~sym)
                         ~sym
                         (let [lsym# (gensym ~(name sym))]
                           (.add symbinds## [lsym# ~sym])
                           lsym#))]))
              bindings)
             `[code## (do ~@body)]])
      (if (< 0 (.size symbinds##))
        (list 'let (into [] cat symbinds##)
           code##)
        code##))))
