(ns sail.core
  (:require [quil.core :as q]
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
  {:time 0})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  (update state :time inc))

(defn scalar-field
  [p]
  p)

(defn range->x [i] (* (/ i num-cells) width))
(defn range->y [j] (* (/ j num-cells) height))

(defn cell-pos
  []
  (let [half (/ num-cells 2)]
    (for [i (range (- half) half)
          j (range (- half) half)]
      [(range->x i) (range->y j)])))

(defn pos->screen
  [[x y]]
  [(* screen-width (+ (/ x width) 0.5))
   (* screen-height (+ (/ y height) 0.5))])

(defn draw-field-pos
  [pos]
  (let [[x y] (pos->screen pos)]
    (q/ellipse x y 2 2)))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  (q/stroke 0 0 0)
;  (doseq
;    [] 
;    draw-field-pos
;    (cell-pos)))
  (doseq
    [pos (cell-pos)]
    (draw-field-pos pos)))

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
