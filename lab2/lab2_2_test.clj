(ns lab2_2_test
  (:use lab2_2)
  (:require [clojure.test :as test]))

(test/deftest defpackage-test
  (test/testing "Testing defpackage"
    (test/is (= 0 ((get-integral #(* 3 %)) 0)))
    (test/is (= 150 ((get-integral #(* 3 %)) 10)))
    (test/is (= -150 ((get-integral #(* -3 %)) 10)))
    (test/is (= 80 ((get-integral #(+ 3 %)) 10)))
    (test/is (= 335 ((get-integral #(* % %)) 10)))))

(test/run-tests 'lab2_2_test)