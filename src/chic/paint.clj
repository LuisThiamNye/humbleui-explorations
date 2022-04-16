(ns chic.paint
  (:require
   [chic.util :as util]
   [better-cond.core :refer [cond] :rename {cond cond+}]
   [clj-commons.primitive-math :as prim]
   [clojure.math :as math]
   [io.github.humbleui.paint :as huipaint]))

(def transparent (huipaint/fill 0x00000000))

;; see https://bottosson.github.io/posts/colorpicker/
;; https://github.com/bottosson/bottosson.github.io/blob/master/misc/colorpicker/colorconversion.js

(deftype MaxSaturationCoefficients
  [^double k0 ^double k1 ^double k2 ^double k3
   ^double k4 ^double wl ^double wm ^double ws])

(def red-max-saturation-coeffs
  (->MaxSaturationCoefficients
   1.19086277 1.76576728 0.59662641 0.75515197
   0.56771245 4.0767416621 -3.3077115913 0.2309699292))

(def green-max-saturation-coeffs
  (->MaxSaturationCoefficients
   0.73956515 -0.45954404 0.08285427 0.12541070
   0.14503204 -1.2684380046 2.6097574011 -0.3413193965))

(def blue-max-saturation-coeffs
  (->MaxSaturationCoefficients
   1.35733652 -0.00915799 -1.15130210 -0.50559606
   0.00692167 -0.0041960863 -0.7034186147 1.7076147010))

(defn compute-max-saturation ^double [^double a ^double b]
  (let [^MaxSaturationCoefficients coeffs
        (cond+
         (prim/< 1. (prim/- (prim/* a -1.88170328)
                            (prim/* b 0.80936493)))
         red-max-saturation-coeffs
         (prim/< 1. (prim/- (prim/* a 1.81444104)
                            (prim/* b 1.19445276)))
         green-max-saturation-coeffs
         blue-max-saturation-coeffs)
        s (prim/+ (.-k0 coeffs)
                  (prim/* a (.-k1 coeffs))
                  (prim/* b (.-k2 coeffs))
                  (prim/* a a (.-k3 coeffs))
                  (prim/* a b (.-k4 coeffs)))
        k-l (prim/+ (prim/* a 0.3963377774) (prim/* b 0.2158037573))
        k-m (prim/+ (prim/* a -0.1055613458) (prim/* b -0.0638541728))
        k-s (prim/+ (prim/* a -0.0894841775) (prim/* b -1.2914855480))
        l' (prim/+ 1. (prim/* s k-l))
        m' (prim/+ 1. (prim/* s k-m))
        s' (prim/+ 1. (prim/* s k-s))
        l (prim/* l' l' l')
        m (prim/* m' m' m')
        s (prim/* s' s' s')
        l-ds (prim/* 3 k-l l' l')
        m-ds (prim/* 3 k-m m' m')
        s-ds (prim/* 3 k-s s' s')
        l-ds2 (prim/* 6 k-l k-l l')
        m-ds2 (prim/* 6 k-m k-m m')
        s-ds2 (prim/* 6 k-s k-s s')
        wl ^double (.-wl coeffs)
        wm ^double (.-wm coeffs)
        ws ^double (.-ws coeffs)
        f (prim/+ (prim/* wl l) (prim/* wm m) (prim/* ws s))
        f1 (prim/+ (prim/* wl l-ds) (prim/* wm m-ds) (prim/* ws s-ds))
        f2 (prim/+ (prim/* wl l-ds2) (prim/* wm m-ds2) (prim/* ws s-ds2))]
    (prim/- s (prim/* f (prim// f1
                                (prim/- (prim/* f1 f1)
                                        (prim/* 0.5 f f2)))))))

(deftype DoubleTuple3 [^double x ^double y ^double z])
(deftype DoubleTuple2 [^double x ^double y])


(defmethod print-method DoubleTuple3 [^DoubleTuple3 o ^java.io.Writer w]
  (doto w
    (.write "#")
    (.write (util/compile (.getName DoubleTuple3)))
    (.write "[")
    (.write (str (.-x o)))
    (.write " ")
    (.write (str (.-y o)))
    (.write " ")
    (.write (str (.-z o)))
    (.write "]")))

(defn ^{:tag `DoubleTuple3} oklab->linear-srgb [^double l ^double a ^double b]
  (let [l' (prim/+ l (prim/* a 0.3963377774) (prim/* b 0.2158037573))
        m' (prim/+ l (prim/* a -0.1055613458) (prim/* b -0.0638541728))
        s' (prim/+ l (prim/* a -0.0894841775) (prim/* b -1.2914855480))
        l (prim/* l' l' l')
        m (prim/* m' m' m')
        s (prim/* s' s' s')]
    (->DoubleTuple3
     (prim/+ (prim/* l 4.0767416621) (prim/* m -3.3077115913) (prim/* s 0.2309699292))
     (prim/+ (prim/* l -1.2684380046) (prim/* m 2.6097574011) (prim/* s -0.3413193965))
     (prim/+ (prim/* l -0.0041960863) (prim/* m -0.7034186147) (prim/* s 1.7076147010)))))

(defn ^{:tag `DoubleTuple2} find-cusp [^double a ^double b]
  (let [s-cusp (compute-max-saturation a b)
        rgb-at-max (oklab->linear-srgb 1. (prim/* s-cusp a) (prim/* s-cusp b))
        l-cusp (Math/cbrt (prim// 1. (prim/max (.-x rgb-at-max)
                                               (.-y rgb-at-max)
                                               (.-z rgb-at-max))))
        c-cusp (prim/* l-cusp s-cusp)]
    (->DoubleTuple2 l-cusp c-cusp)))

(defn ^{:tag `DoubleTuple2} get-st-max [^double a ^double b]
  (let [cusp (find-cusp a b)
        l (.-x cusp)
        c (.-y cusp)]
    (->DoubleTuple2 (prim// c l) (prim// c (prim/- l 1.)))))

(defn srgb-transfer-function ^double [^double x]
  (if (prim/<= x 0.0031308)
    (prim/* x 12.92)
    (prim/- (prim/* 1.055 (Math/pow x 0.4166666666666667))
            0.055)))

(defn toe-inv ^double [^double x]
  (let [k1 0.206
        k2 0.03
        k3 (prim// (prim/+ 1. k1) (prim/+ 1. k2))]
    (prim// (prim/+ (prim/* x x) (prim/* x k1))
            (prim/* k3 (prim/+ x k2)))))

(defn ^{:tag `DoubleTuple3} okhsv->srgb [^double h ^double s ^double v]
  (let [a (Math/cos (prim/* 2. math/PI h))
        b (Math/sin (prim/* 2. math/PI h))
        st-max (get-st-max a b)
        s-max (.-x st-max)
        s0 0.5
        t (.-y st-max)
        k (prim/- 1. (prim// s0 s-max))
        l-v (prim/- 1. (prim/* s (prim// s0 (prim/- (prim/+ s0 t)
                                                    (prim/* t k s)))))
        c-v (prim// (prim/* s t s0)
                    (prim/- (prim/+ s0 t)
                            (prim/* t k s)))
        l (prim/* v l-v)
        c (prim/* v c-v)
        l-vt ^double (toe-inv l-v)
        c-vt (prim/* c-v (prim// l-vt l-v))
        l-new ^double (toe-inv l)
        c (prim/* c (prim// l-new l))
        l l-new
        rgb-scale (oklab->linear-srgb l-vt (prim/* a c-vt) (prim/* b c-vt))
        scale-l (Math/cbrt (prim// 1. (prim/max 0. (.-x rgb-scale) (.-y rgb-scale) (.-z rgb-scale))))
        l (prim/* l scale-l)
        c (prim/* c scale-l)
        rgb (oklab->linear-srgb l (prim/* c a) (prim/* c b))]
    (->DoubleTuple3
     (prim/* 255. ^double (srgb-transfer-function (.-x rgb)))
     (prim/* 255. ^double (srgb-transfer-function (.-y rgb)))
     (prim/* 255. ^double (srgb-transfer-function (.-z rgb))))))

(defn okhsva* ^long [^double h ^double s ^double v ^double a]
  (let [rgb (okhsv->srgb h s v)]
    (prim/+
     (prim/<< (Math/round (prim/* 255. a)) 24)
     (prim/<< (Math/round (.-x rgb)) 16)
     (prim/<< (Math/round (.-y rgb)) 8)
     (Math/round (.-z rgb)))))

(defn okhsv* ^long [^double h ^double s ^double v]
  (okhsva* h s v 1.))

(defmacro okhsv [h s v]
  (okhsv* h s v))

(defmacro okhsva [h s v a]
  (okhsva* h s v a))
