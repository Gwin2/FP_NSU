(ns lab-2-2.core-test
  (:require [clojure.test :refer :all]
            [lab-2-2.core :refer :all]))

(defn abs
  [x]
  (if (> 0 x) x (* -1 x)))

(defn equal-eps
  [x y eps]
  (if (> (abs (- x y)) eps) false true))

(deftest integral-test-seq
  (testing "sequence way"
           (is (= true (equal-eps 25.0 ((calc-integral-seq (fn [x] (* 2 x)) 0.1) 5) 0.001)))
           (is (= true (equal-eps 9.0 ((calc-integral-seq (fn [x] (* x x)) 0.1) 3) 0.001)))
           (is
            (= true
               (equal-eps 682.666 ((calc-integral-seq (fn [x] (* x x x x x)) 0.1) 4) 0.001)))))
