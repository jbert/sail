(ns sail.core
  (:require [quil.core :as q]
            [clojure.math.numeric-tower :as math]
            [quil.middleware :as m]))

(def screen-width 400)
(def screen-height 400)

(def pixels-per-metre 1)

(def ticks-per-sec 20)

;; Our arena is a world with the origin at the centre
(def width (/ screen-width pixels-per-metre 2))
(def height (/ screen-height pixels-per-metre 2))
(def num-cells 51)

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

(defn scalar-field
  [[x y] tick]
  (let [theta (+ (safe-atan x y)
                 (* Math/PI
                    (/ tick (* 5 ticks-per-sec))))]
    (* (Math/abs (Math/sin theta))
       (math/sqrt (+ (* x x)
                     (* y y))))))

(defn setup []
  (q/frame-rate ticks-per-sec)
  (let [field-vals (map #(scalar-field % 0) (cell-pos))]
    {:tick 0
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

(defn draw-field-pos
  [[min-val max-val] f pos tick]
  (let [[x y] (pos->screen pos)
        box-width (/ screen-width num-cells)
        box-height (/ screen-width num-cells)
        current-val (f pos tick)]
    ;    (q/ellipse x y 5 5)))
    (q/with-fill (field->colour min-val max-val current-val)
      (centered-rect x y box-width box-height))
    current-val))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (let [range-ref (:range-ref state)]
    (q/background 240)
    (q/stroke 0 0 0)
    (let [new-vals (map #(draw-field-pos (deref range-ref)
                                         scalar-field
                                         %
                                         (:tick state))
                        (cell-pos))]
      (swap! range-ref (fn [old-val] [(apply min new-vals) (apply max new-vals)])))))

(q/defsketch sail
  :title "You spin my circle right round"
  :size [screen-width screen-height]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])


(defn -main
  ""
  []
  true)
