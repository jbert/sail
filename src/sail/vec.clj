(ns sail.vec
  (:require [clojure.math.numeric-tower :as math]))

;; Vec is a hash of {:x x :y y} or {:r r :theta theta} and we switch between them
;; depending on the last operation, cacheing as needed
;; if (x,y) or (r,t) are present, they are accurate and may be used. We discard
;; (rather than calculate) when we derive a new value.

(defn make-vec
  [x y]
  {:x x :y y})

(defn vec-scale
  [{x :x y :y r :r t :t} scale]
  (if (nil? x)
    {:r (* r scale) :t t}
    {:x (* scale x) :y (* scale y)}))

(defn vec-length
  [{x :x y :y r :r t :t}]
  (if (nil? x)
    r
    (math/sqrt (+ (* x x) (* y y)))))

