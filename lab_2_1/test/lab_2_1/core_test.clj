(ns lab-2-1.core-test
  (:require [clojure.test :refer :all]
            [lab-2-1.core :refer :all]))

(def step 0.25)

(defn abs
  [x]
  (if (> 0 x) x (* -1 x)))

(defn equal-eps
  [x y eps]
  (if (> (abs (- x y)) eps) false true))

(deftest integral-test-mem
  (testing "memoized way"
           (is (= true (equal-eps 25.0 ((calc-integral-mem (fn [x] (* 2 x)) 0.1) 5) 0.001)))
           (is (= true (equal-eps 9.0 ((calc-integral-mem (fn [x] (* x x)) 0.1) 3) 0.001)))
           (is
             (= true
                (equal-eps 682.666 ((calc-integral-mem (fn [x] (* x x x x x)) 0.1) 4) 0.001)))))
