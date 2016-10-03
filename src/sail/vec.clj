(ns sail.vec
  (:require [clojure.math.numeric-tower :as math]))

;; Vec is a hash of {:x x :y y} or {:r r :theta theta} and we switch between them
;; depending on the last operation, cacheing as needed
;; if (x,y) or (r,t) are present, they are accurate and may be used. We discard
;; (rather than calculate) when we derive a new value.

(defn make-vec
  [x y]
  {:x x :y y})

(defn cart-polr-dispatch
  [{x :x y :y r :r t :t last-conv :last-conv} & args ]
  (if (not (nil? last-conv))
    last-conv
    (if (nil? x)
      :polr
      :cart)))

(defn xy-to-theta
  [x y]
  (let [epsilon 1e-9
        pi2 (/ Math/PI 2)]
    (if (< x epsilon)
      (if (< y epsilon)
        ; 0,0
        0
        (if (pos? y)
          pi2
          (- pi2)))
      ; (atan x) run from -pi/2 -> pi/2
      ; inspect overall sign of x & y to determine full -pi -> pi range
      (let [atanyx (Math/atan (/ y x))]
        (if (pos? x)
          atanyx
          (+ atanyx Math/PI))))))

(defn to-cart
  [{x :x y :y r :r t :t :as v}]
  (if (not (nil? x))
    v
    {:last-conv :cart 
     :r r
     :t t
     :x (* r (Math/cos t))
     :y (* r (Math/sin t))}))

(defn to-polr
  [{x :x y :y r :r t :t :as v}]
  (if (not (nil? r))
    v
    {:last-conv :polr
     :r (math/sqrt (+ (* x x)
                      (* y y)))
     :t (xy-to-theta x y)
     :x x
     :y y}))

(defn approx=
  [a b]
  (let [epsilon 1e-9]
    (< (- a b)
       epsilon)))

(defn equal
  [a b]
  (let [ca (to-cart a)
        cb (to-cart b)]
;    (println "JB" (float (:x ca)) (float (:x cb))
;             (float (:y ca)) (float (:y cb)))
    (and (approx= (float (:x ca)) (float (:x cb)))
         (approx= (float (:y ca)) (float (:y cb))))))

; Easy way to declare multimethod for cartesion and polar representations
(defmacro cart-and-polr
  [fn-name arglist cart-impl polr-impl]
  `(do
     (defmulti ~fn-name cart-polr-dispatch)
     (defmethod ~fn-name
       :cart
       [{x :x y :y} ~@arglist]
       ~cart-impl)
     (defmethod ~fn-name
       :polr
       [{r :r t :t} ~@arglist]
       ~polr-impl)))

(cart-and-polr
  scale
  [s]
  {:x (* s x) :y (* s y)}
  {:r (* r s) :t t})

(cart-and-polr
  length
  []
  (length (to-polr {:x x :y y}))
  r)

(cart-and-polr
  add
  [b]
  (let [cb (to-cart b)]
    {:x (+ x (:x cb))
     :y (+ y (:y cb))})
  (add (to-cart {:r r :t t}) b))
