(ns sail.core
  (:require [quil.core :as q]
            [clojure.math.numeric-tower :as math]
            [quil.middleware :as m]))

(def screen-width 400)
(def screen-height 400)

(def pixels-per-metre 1)

;; Our arena is a world with the origin at the centre
(def width (/ screen-width pixels-per-metre 2))
(def height (/ screen-height pixels-per-metre 2))
(def num-cells 10)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 1)
  {:tick 0
   :range-ref (atom [0 screen-width])})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  (update state :tick inc))

(defn irange->x
  [i]
  (let [cell-width (/ width num-cells)]
    (* i
       cell-width)))

(defn irange->y
  [j]
  (let [cell-height (/ height num-cells)]
    (* j
       cell-height)))

(defn cell-pos
  []
  (let [half (/ num-cells 2)]
    (for [i (range (- half) half)
          j (range (- half) half)]
      [(irange->x i) (irange->y j)])))

(defn pos->screen
  [[x y]]
  [(* screen-width (+ (/ x width) 0.5))
   (* screen-height (+ (/ y height) 0.5))])

(defn scalar-field
  [[x y]]
  (math/sqrt (+ (* x x)
                (* y y))))

(defn field->colour
  [minv maxv v]
  (let [intensity (* 255 (/ (- v minv) maxv))]
    [intensity intensity intensity]))


(defn draw-field-pos
  [min-val max-val f pos]
  (let [[x y] (pos->screen pos)
        box-width (/ screen-width num-cells)
        box-height (/ screen-width num-cells)
        current-val (f pos)]
;    (q/ellipse x y 5 5)))
    (q/with-fill (field->colour min-val max-val current-val)
      (q/rect x y box-width box-height))
    current-val))
    

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (let [range-ref (:range-ref state)]
    (q/background 240)
    (q/stroke 0 0 0)
    ;  (doseq
    ;    [] 
    ;    draw-field-pos
    ;    (cell-pos)))
    (let [[min-val max-val] (deref range-ref)
          new-vals (map #(draw-field-pos min-val max-val scalar-field %) (cell-pos))]
      (swap! range-ref (fn [old-val] [(apply min new-vals) (apply max new-vals)])))))
;      [pos (cell-pos)]
;      (draw-field-pos (ref max-val-ref) scalar-field pos))))

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
