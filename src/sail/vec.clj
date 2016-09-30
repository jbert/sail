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
  [{x :x y :y r :r t :t} & args ]
  (if (nil? x)
    :polr
    :cart))

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

(defn to-polr
  [{x :x y :y}]
  {:r (math/sqrt (+ (* x x)
                    (* y y)))
   :t (xy-to-theta x y)})

;(defmacro cart-and-polr [fn-name arglist cart-impl polr-impl] `((defn ~fn-name [] 1) `(defn foo [] 1)))
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

(defn add
  [vx vy]
  )
