(ns chic.humbleui
  (:require
   [io.github.humbleui.core :as hui]
   [clojure.set :as set]))

;; Adapted from https://github.com/HumbleUI/HumbleUI/blob/HEAD/src/io/github/humbleui/core.clj
;; To allow more type hints in the fields

(defmacro deftype+
  "Same as deftype, but:

   1. Uses ^:mut instead of ^:unsynchronized-mutable
   2. Allows using type annotations in protocol arglist
   3. Read mutable fields through ILookup: (:field instance)
   4. Write to mutable fields from outside through ISettable: (-set! instance key value)
   5. Allow with-meta"
  [name fields & body]
  (let [update-field #(vary-meta % set/rename-keys {:mut :unsynchronized-mutable})
        remove-tag #(vary-meta % dissoc :tag)
        update-method (fn [[name args & body]]
                        (list name
                              (mapv remove-tag args)
                              (list* 'clojure.core/let
                                     (vec (mapcat #(vector % (remove-tag %)) (filter #(:tag (meta %)) args)))
                                     body)))
        value-sym (gensym 'value)]
    `(do
       (deftype ~name
         ~(mapv update-field (conj fields '__m))

         ~@(map #(if (list? %) (update-method %) %) body)

         clojure.lang.IMeta
         (meta [_] ~'__m)

         clojure.lang.IObj
         (withMeta [_ meta#]
           (new ~name ~@fields meta#))

         clojure.lang.ILookup
         (valAt [_# key# notFound#]
           (case key#
             ~@(mapcat #(vector (keyword %) %) fields)
             notFound#))
         (valAt [this# key#]
           (.valAt this# key# nil))

         hui/ISettable
         (-set! [_# key# ~value-sym]
           (case key#
             ~@(mapcat #(vector (keyword %) (list 'set! % value-sym)) (filter #(:mut (meta %)) fields)))))
       (defn ~(symbol (str '-> name)) ~(mapv (fn [field]
                                               (let [m (meta field)]
                                                 (if (#{'boolean} (:tag m))
                                                   (vary-meta field dissoc :tag)
                                                   field)))
                                             fields)
         (new ~name ~@fields nil)))))

(comment
  (#{'boolean}
   (:tag (meta (first(nth (first (nnext (macroexpand-1 '(deftype+ j [^boolean XXXX]))))
                     2)))))
  #!
  )
