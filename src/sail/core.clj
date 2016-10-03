(ns sail.core
  (:require [quil.core :as q]
            [clojure.math.numeric-tower :as math]
            [sail.vec :as v]
            [quil.middleware :as m]))

(def screen-width 800)
(def screen-height 800)

(def pixels-per-metre 1)

(def ticks-per-sec 10)
(def num-cells 51)

;; Our arena is a world with the origin at the centre
(def width (/ screen-width pixels-per-metre 2))
(def height (/ screen-height pixels-per-metre 2))

(defn irange->x
  [i]
  (let [cell-width (/ width num-cells)]
    (* (+ i 0.5)
       cell-width)))

(defn irange->y
  [j]
  (let [cell-height (/ height num-cells)]
    (* (+ j 0.5)
       cell-height)))

(defn cell-pos
  []
  (let [half (/ num-cells 2)]
    (for [i (range (- half) half)
          j (range (- half) half)]
      [(irange->x i) (irange->y j)])))

(defn safe-atan
  [x y]
  (let [epsilon 1e-9]
    (if (< (Math/abs x) epsilon)
      (let [pi2 (/ Math/PI 2)]
        (if (pos? y)
          pi2
          (- pi2)))
      (Math/atan (/ y x)))))

;(defn scalar-field
;  [tick [x y]]
;  (let [v (+ x y)]
;    (+ v (Math/sin (/ v 2)))))

(defn scalar-field
  [tick [x y]]
  (let [theta (* Math/PI 2 (/ x width))
        gamma (* Math/PI 2 (/ y height))
        phi (/ (* Math/PI 2 (/ tick ticks-per-sec)) 5)]
    (+ (Math/sin (+ phi (* 3 (+ theta))))
       (Math/sin (+ phi (* 3 (+ gamma)))))))

;(defn scalar-field
;  [tick [x y]]
;  (let [theta (+ (safe-atan x y)
;                 (* Math/PI
;                    (/ tick (* 5 ticks-per-sec))))]
;    (* (Math/abs (Math/sin theta))
;       (math/sqrt (+ (* x x)
;                     (* y y))))))

(defn make-boat
  [name x y]
  {:name name
   :pos [x y]})

(defn setup []
  (q/frame-rate ticks-per-sec)
  (let [field-vals (map #(scalar-field 0 %) (cell-pos))]
    {:tick 0
     :boat (make-boat "Badger" 0 0)
     :range-ref (atom [(apply min field-vals) (apply max field-vals)])}))

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  (update state :tick inc))

(defn pos->screen
  [[x y]]
  [(* screen-width (+ (/ x width) 0.5))
   (* screen-height (+ (/ y height) 0.5))])

(defn field->colour
  [minv maxv v]
  (if (= minv maxv)
    [0 0 0]
    (let [intensity (* 255 (/ (- v minv) (- maxv minv)))]
      [intensity intensity intensity])))

(defn centered-rect
  "Same as quil/rect, but x y is the box centre"
  [cx cy w h]
  (q/rect (- cx (/ w 2))
          (- cy (/ h 2))
          w
          h))

(defn vector-derivative
  [tick sf pos]
  (let [step 1
        sample-points (map #(v/scale % step) [[1 0] [0 1] [-1 0] [0 -1]])
        sval (sf tick pos)
        deltas (map #(v/scale % (/ (- (sf tick (v/add pos %))
                                      sval)
                                   step))
                    sample-points)]
    (reduce v/add deltas)))

;(defn vector-field
;  [tick [x y]]
;  [5 5])

(defn quil-draw-vec
  [cx cy [vx vy]]
  (q/ellipse cx cy 2 2)
  (q/line cx cy (+ cx vx) (+ cy vy)))

(defn draw-vector-field-at-pos
  [[x y] vval]
  (q/with-stroke [255 0 0]
    (q/with-fill [255 0 0]
      (quil-draw-vec x y (v/scale vval 100)))))

(defn draw-scalar-field-at-pos
  [[x y] sval [min-val max-val]]
  (let [box-width (/ screen-width num-cells)
        box-height (/ screen-width num-cells)]
    (q/with-fill (field->colour min-val max-val sval)
      (centered-rect x y box-width box-height))))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (let [range-ref (:range-ref state)]
    (q/background 240)
    (q/stroke 0 0 0)
    (let [tick (:tick state)
;          new-vals (map #(let [sval (scalar-field tick %)
;                               vval (vector-derivative tick scalar-field %)
;                               screen-pos (pos->screen %)]
;                           (draw-scalar-field-at-pos screen-pos sval (deref range-ref))
;                           (draw-vector-field-at-pos screen-pos vval)
;                           sval)
;                        (cell-pos))]
;      (swap! range-ref (fn [old-val] [(apply min new-vals) (apply max new-vals)])))))
          ])))

;(q/defsketch sail
;  :title "You spin my circle right round"
;  :size [screen-width screen-height]
;  :setup setup
;  :update update-state
;  :draw draw-state
;  :middleware [m/fun-mode])


(defn -main
  ""
  []
  true)
