(ns sail.vec
  (:require [clojure.math.numeric-tower :as math]))

;; Vec is a hash of {:x x :y y} or {:r r :theta theta} and we switch between them
;; depending on the last operation, cacheing as needed
;; if (x,y) or (r,t) are present, they are accurate and may be used. We discard
;; (rather than calculate) when we derive a new value.

(defn make-vec
  [x y]
  {:x x :y y})

(defn cart-or-polr
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

(defmulti scale cart-or-polr)
(defmethod scale
  :cart
  [{x :x y :y} scale]
  {:x (* scale x) :y (* scale y)})
(defmethod scale
  :polr
  [{r :r t :t} scale]
  {:r (* r scale) :t t})

;(defn scale
;  [{x :x y :y r :r t :t} scale]
;  (if (nil? x)
;    {:r (* r scale) :t t}
;    {:x (* scale x) :y (* scale y)}))

(defmulti length cart-or-polr)
(defn length-polr
  [{r :r t :t}]
  r)
(defmethod length
  :cart
  [v]
  ((comp length-polr to-polr) v))
(defmethod length
  :polr
  [& args]
  (apply length-polr args))
;(defn length
;  [{x :x y :y r :r t :t}]
;  (if (nil? x)
;    r
;    (math/sqrt (+ (* x x) (* y y)))))


(defn add
  [vx vy]
  )
