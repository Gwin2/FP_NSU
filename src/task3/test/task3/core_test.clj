(ns task3.core-test
  (:require [clojure.test :refer :all]
            [task3.core :refer :all]))

(defn slowFunc
  "'Slowly working' filter func, to see the optimisation of separation"
  [val]
  (Thread/sleep 9)
  (= (mod val 4) 0))

(deftest emptyTest (is (= (filter odd? (range -1)) (filter odd? (range 0)))))
(deftest valTest (is (= (filter odd? (range -1 1000)) (filter odd? (range 0 1000)))))

(deftest timetest (time (doall (take 99 (filter slowFunc (iterate inc 0)))))
                  (time (doall (take 99 (pfilter slowFunc (iterate inc 0))))))
