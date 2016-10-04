(ns sail.vec-test
  (:require [clojure.test :refer :all]
            [clojure.math.numeric-tower :as math]
            [sail.vec :as v]))

(def v0-0 (v/make-vec 0 0))
(def v0-1 (v/make-vec 0 1))
(def v1-0 (v/make-vec 1 0))
(def v1-1 (v/make-vec 1 1))
(def v0-5 (v/make-vec 0 5))
(def v5-0 (v/make-vec 5 0))
(def v3-4 (v/make-vec 3 4))
(def v5-12 (v/make-vec 5 12))

(def vr0-0 (v/to-polr v0-0))
(def vr0-1 (v/to-polr v0-1))
(def vr1-0 (v/to-polr v1-0))
(def vr0-5 (v/to-polr v0-5))
(def vr5-0 (v/to-polr v5-0))
(def vr3-4 (v/to-polr v3-4))
(def vr5-12 (v/to-polr v5-12))


(deftest convert
  (testing "some basic cartesian/polar conversions"
    (is (v/equal {:r 1 :t 0}
           (v/to-polr (v/make-vec 1 0))))
    (is (v/equal {:r (math/sqrt 2) :t (/ Math/PI 4)}
           (v/to-polr v1-1)))
    (is (v/equal v5-0 vr5-0))
    (is (v/equal v0-5 vr0-5))
    (is (v/equal v0-5 vr0-5))
    (is (v/equal {:r 0 :t 0}
           (v/to-polr v0-0)))))

(deftest vec-length-test
  (testing "Cartesian length tests"
    (is (= (v/length v0-5) 5))
    (is (= (v/length v5-0) 5))
    (is (= (v/length v3-4) 5))
    (is (= (v/length v5-12) 13)))
  (testing "Polar length tests"
    (is (= (v/length vr0-5) 5))
    (is (= (v/length vr5-0) 5))
    (is (= (v/length vr3-4) 5))
    (is (= (v/length vr5-12) 13))))

(deftest vec-scale-test
  (testing "Cartesian scale tests"
    (is (v/equal (v/scale v0-5 5)
           (v/make-vec 0 25)))
    (is (v/equal (v/scale v0-5 (/ 1 5))
           (v/make-vec 0 1)))
    (is (v/equal (v/scale v3-4 2)
           (v/make-vec 6 8))))
  (testing "Polar scale tests"
    (is (v/equal (v/scale vr0-5 5)
           (v/make-vec 0 25)))
    (is (v/equal (v/scale vr0-5 (/ 1 5))
           (v/make-vec 0 1)))
    (is (v/equal (v/scale vr3-4 2)
           (v/make-vec 6 8)))))

(deftest vec-add-test
  (testing "cartesian"
    (is (v/equal (v/add v0-1 v0-1) (v/make-vec 0 2)))
    (is (v/equal (v/add v0-1 v1-0) (v/make-vec 1 1))))
  (testing "polar"
    (is (v/equal (v/add vr0-1 vr0-1) (v/make-vec 0 2)))
    (is (v/equal (v/add vr0-1 vr1-0) (v/make-vec 1 1))))
  (testing "mixed"
    (is (v/equal (v/add vr0-1 v0-1) (v/make-vec 0 2)))
    (is (v/equal (v/add vr0-1 v1-0) (v/make-vec 1 1)))))

(def pi4 (/ Math/PI 4))
(def pi2 (/ Math/PI 2))

(deftest vec-turn-test
  (testing "cartesian"
    (is (v/equal (v/turn v1-0 pi2) (v/make-vec 0 1)))))
