(ns sail.vec-test
  (:require [clojure.test :refer :all]
            [sail.vec :refer :all]))

(deftest vec-cartesian
  (testing "Basic tests for macro"
    (is (= (vec-length (make-vec 0 5)) 5))
    (is (= (vec-length (make-vec 5 0)) 5))
    (is (= (vec-length (make-vec 3 4)) 5))
    (is (= (vec-length (make-vec 5 12)) 13))))
