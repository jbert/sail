(ns sail.core
  (:require [quil.core :as q]
            [clojure.math.numeric-tower :as math]
            [sail.vec :as v]
            [quil.middleware :as m]))

(def screen-width 800)
(def screen-height 800)

(def pixels-per-metre 1)

(def ticks-per-sec 1)
(def num-cells 21)

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
      (v/make-vec (irange->x i) (irange->y j)))))

;(defn safe-atan
;  [x y]
;  (let [epsilon 1e-9]
;    (if (< (Math/abs x) epsilon)
;      (let [pi2 (/ Math/PI 2)]
;        (if (pos? y)
;          pi2
;          (- pi2)))
;      (Math/atan (/ y x)))))

;(defn scalar-field
;  [tick [x y]]
;  (let [v (+ x y)]
;    (+ v (Math/sin (/ v 2)))))

(defn scalar-field
  [tick pos]
  (let [{x :x y :y} (v/to-cart pos)]
    (let [theta (* Math/PI 2 (/ x width))
          gamma (* Math/PI 2 (/ y height))
          phi (/ (* Math/PI 2 (/ tick ticks-per-sec)) 5)]
      (+ (Math/sin (+ phi (* 3 (+ theta))))
         (Math/sin (+ phi (* 3 (+ gamma))))))))

;(defn scalar-field
;  [tick [x y]]
;  (let [theta (+ (safe-atan x y)
;                 (* Math/PI
;                    (/ tick (* 5 ticks-per-sec))))]
;    (* (Math/abs (Math/sin theta))
;       (math/sqrt (+ (* x x)
;                     (* y y))))))

(defn make-boat
  [name pos dir]
  {:name name
   :pos pos
   :dir dir
   :length (/ screen-width 10)
   :width (/ screen-width 40)
   })

(defn setup []
  (q/frame-rate ticks-per-sec)
  (let [field-vals (map #(scalar-field 0 %) (cell-pos))]
    {:tick 0
     :boat (make-boat "Badger" (v/make-vec 0 0) (v/make-vec 1 0))
     :range-ref (atom [(apply min field-vals) (apply max field-vals)])}))

(defn handle-keypress
  [keypress boat]
  (let [radians-per-second (/ Math/PI 5)
        radians-per-keypress (/ radians-per-second ticks-per-sec)]
    (cond
      (= keypress :left)
      (update boat :dir v/turn radians-per-keypress)
      (= keypress :right)
      (update boat :dir v/turn (- radians-per-keypress))
      :else
      boat)))

(defn update-boat
  [b]
  (if (q/key-pressed?)
    (handle-keypress (q/key-as-keyword) b)
    b))


(defn update-state
  [state]
  (-> state
      (update :tick inc)
      (update :boat update-boat)))

(defn pos->screen
  [pos]
  (let [{x :x y :y} (v/to-cart pos)]
    (v/make-vec (* screen-width (+ (/ x width) 0.5))
                (* screen-height (+ (/ y height) 0.5)))))

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
        sample-points (map (fn [[x y]] (v/scale (v/make-vec x y)
                                                step))
                           [[1 0] [0 1] [-1 0] [0 -1]])
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
  [pos v]
  (let [{x :x y :y} (v/to-cart pos)
        {vx :x vy :y} (v/to-cart v)]
    (q/ellipse x y 2 2)
    (q/line x y (+ x vx) (+ y vy))))

(defn draw-boat
  [b]
  (let [{x :x y :y} (pos->screen (v/to-cart (:pos b)))
        {theta :t} (v/to-polr (:dir b))]
    (q/with-translation [x y]
      (q/with-rotation [theta]
        (q/with-stroke [0 0 255]
          (q/with-fill [0 0 255]
            (let [wb (/ (:width b) 1.5)
                  ws (/ wb 2)
                  l2 (/ (:length b) 2)
                  l4 (/ l2 2)
                  bow [l2 0]
                  s+ [(- l2) ws]
                  s- [(- l2) (- ws)]
                  b+ [0 wb]
                  b- [0 (- wb)]]
              (apply q/line (concat bow b+))
              (apply q/line (concat b+ s+))
              (apply q/line (concat s+ s-))
              (apply q/line (concat s- b-))
              (apply q/line (concat b- bow)))))))))


(defn draw-vector-field-at-pos
  [pos vval]
  (q/with-stroke [255 0 0]
    (q/with-fill [255 0 0]
      (quil-draw-vec pos (v/scale vval 100)))))

(defn draw-scalar-field-at-pos
  [pos sval [min-val max-val]]
;  (println "JB" pos sval min-val max-val)
  (let [{x :x y :y} (v/to-cart pos)]
    (let [box-width (/ screen-width num-cells)
          box-height (/ screen-width num-cells)]
      (q/with-fill (field->colour min-val max-val sval)
        (centered-rect x y box-width box-height)))))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (let [range-ref (:range-ref state)]
    (q/background 240)
    (q/stroke 0 0 0)
    (let [tick (:tick state)
          new-vals (map #(let [sval (scalar-field tick %)
                               vval (vector-derivative tick scalar-field %)
                               screen-pos (pos->screen %)]
;                           (draw-scalar-field-at-pos screen-pos sval (deref range-ref))
;                           (draw-vector-field-at-pos screen-pos vval)
                           sval)
                        (cell-pos))]
      (draw-boat (:boat state))
      (swap! range-ref (fn [old-val] [(apply min new-vals) (apply max new-vals)])))))
  ;        ])))

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
