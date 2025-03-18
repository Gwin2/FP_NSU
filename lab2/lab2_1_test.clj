(ns lab2_1_test
  (:use lab2_1)
  (:require [clojure.test :as test]))

(test/deftest defpackage-test
  (test/testing "Testing defpackage"
    (test/is (= 0 ((integral-mem #(* 3 %)) -2)))
    (test/is (= 0 ((integral-mem #(* 3 %)) 0)))
    (test/is (= 150 ((integral-mem #(* 3 %)) 10)))
    (test/is (= -150 ((integral-mem #(* -3 %)) 10)))
    (test/is (= 335 ((integral-mem #(* % %)) 10)))))

(test/run-tests 'lab2_1_test)