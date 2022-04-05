(ns chic.debug.swap
  (:require
   [clojure.walk :as walk]
   [chic.debug :as debug]))

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                 (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

(defn can-wrap-in-try? [code]
  (let [*found-offender? (volatile! false)
        syms #{'recur 'set! 'let}]
    (walk/prewalk
     (fn [x]
       (cond
         (and (list? x) (= 'quote (first x)))
         nil
         (syms x)
         (do (vreset! *found-offender? true) nil)
         :else
         x))
     code)
    (not @*found-offender?)))

(defmacro catching-all [& body]
  `(try ~@body
        (catch Throwable e#
          (debug/handle-caught-error e#))))

(defn wrap-expr [expr]
  (cond
    (not (seqable? expr))
    expr
    (can-wrap-in-try? expr)
    `(catching-all ~expr)
    :else
    expr))

(defn install-instrumented-let! []
  (alter-var-root
   #'let
   (fn [_]
     (fn let
       [&form &env bindings & body]
       (assert-args
        (vector? bindings) "a vector for its binding"
        (even? (count bindings)) "an even number of forms in binding vector")
       #_(list* 'let* (destructure bindings) body)
       (let* [bindings' (into []
                              (comp (partition-all 2)
                                    (mapcat (fn [[sym v]]
                                              (let* [tag (:tag (meta v))]
                                                [(cond-> sym (and tag (not (:tag (meta sym))))
                                                         (vary-meta assoc :tag tag))
                                                 (wrap-expr v)]))))
                              (destructure bindings))]
             (list* 'let* bindings' (map wrap-expr body)))))))

(defn install-all-instrumentation! []
  (install-instrumented-let!))

(comment
  (find-recur '(recur))
  (partition 2)
  (def t (Thread.
          (fn []
            (catching-all
             (+ "")))))
  (.start t)

  (catching-all
   (/ 0))

  (macroexpand-1
   '(let []
      (set! x 0)
      4
      ()))
  (defmacro t [] (tap> &env) nil)
  (let [x 4]
    (t))
  (let [x 1]
    (/ 0)
    5)
  (macroexpand-1 '(chic.humbleui/deftype+ x [y] X (doit [_] (set! y 0))))
  #!
  )
