(ns sail.vec-test
  (:require [clojure.test :refer :all]
            [clojure.math.numeric-tower :as math]
            [sail.vec :as v]))

(deftest convert
  (testing "some basic cartesian/polar conversions"
    (is (= {:r 1 :t 0}
           (v/to-polr (v/make-vec 1 0))))
    (is (= {:r (math/sqrt 2) :t (/ Math/PI 4)}
           (v/to-polr (v/make-vec 1 1))))
    (is (= {:r 0 :t 0}
           (v/to-polr (v/make-vec 0 0))))))

(deftest vec-cartesian-length
  (testing "Cartesian length tests"
    (is (= (v/length (v/make-vec 0 5)) 5))
    (is (= (v/length (v/make-vec 5 0)) 5))
    (is (= (v/length (v/make-vec 3 4)) 5))
    (is (= (v/length (v/make-vec 5 12)) 13)))
  (testing "Polar length tests"
    (is (= (v/length (v/to-polr (v/make-vec 0 5))) 5))
    (is (= (v/length (v/to-polr (v/make-vec 5 0))) 5))
    (is (= (v/length (v/to-polr (v/make-vec 3 4))) 5))
    (is (= (v/length (v/to-polr (v/make-vec 5 12))) 13))))
