(ns lab-4.core-test
  (:require [clojure.test :refer :all]
            [lab-4.operations :refer :all]
            [lab-4.init-variables :refer :all]
            [lab-4.dnf-rules :refer :all]))

(deftest test-conjunction
  (testing "x * y = x * y"
           (is
            (= (dnf (conjunction (variable :x) (variable :y)))
               (conjunction (variable :x) (variable :y)))))

  (testing "!(x * y) = !x + !y"
           (is
            (= (dnf (negation (conjunction (variable :x) (variable :y))))
               (disjunction (negation (variable :x)) (negation (variable :y))))))

  (testing "x * x = x"
           (is
            (= (dnf (conjunction (variable :x) (variable :x)))
               (variable :x))))

  (testing "!x * x = 0"
           (is
            (= (dnf (conjunction (negation (variable :x)) (variable :x)))
               constant-false)))

  (testing "x * 1 = x"
           (is
            (= (dnf (conjunction (variable :x) constant-true))
               (variable :x))))

  (testing "x * 0 = 0"
           (is
            (= (dnf (conjunction (variable :x) constant-false))
               constant-false)))

  (testing "x * 1 * z * x * y = x * z * y"
           (is
            (=
             (dnf
              (conjunction (variable :x) constant-true (variable :z) (variable :x) (variable :y)))
             (conjunction (variable :x) (variable :z) (variable :y))))))

(deftest test-disjunction
  (testing "x + y = x + y"
           (is
            (= (dnf (disjunction (variable :x) (variable :y)))
               (disjunction (variable :x) (variable :y)))))

  (testing "!(x + y) = !x * !y"
           (is
            (= (dnf (negation (disjunction (variable :x) (variable :y))))
               (conjunction (negation (variable :x)) (negation (variable :y))))))

  (testing "!(x + y + 0) = !x * !y"
           (is
            (= (dnf (negation (disjunction (variable :x) (variable :y) constant-false)))
               (conjunction (negation (variable :x)) (negation (variable :y))))))

  (testing "x + x = x"
           (is
            (= (dnf (disjunction (variable :x) (variable :x)))
               (variable :x))))

  (testing "!x + x = 1"
           (is
            (= (dnf (disjunction (negation (variable :x)) (variable :x)))
               constant-true)))

  (testing "x + 1 = 1"
           (is
            (= (dnf (disjunction (variable :x) constant-true))
               constant-true)))

  (testing "x + 0 = x"
           (is
            (= (dnf (disjunction (variable :x) constant-false))
               (variable :x))))

  (testing "x + 1 + y + x + z + 0 + x + y = 1"
           (is
            (=
             (dnf
              (disjunction (variable :x) constant-true (variable :y) (variable :x) (variable :z) constant-false (variable :x) (variable :y)))
             constant-true)))

  (testing "x + y + x + z + 0 + x + y = 1"
           (is
            (=
             (dnf
              (disjunction (variable :x) (variable :y) (variable :x) (variable :z) constant-false (variable :x) (variable :y)))
             (disjunction (variable :x) (variable :y) (variable :z))))))

(deftest test-negation
  (testing "!x = !x"
           (is
            (= (dnf (negation (variable :x)))
               (negation (variable :x)))))
  (testing "!!x = x"
           (is
            (= (dnf (negation (negation (variable :x))))
               (variable :x))))
  (testing "!1 = 0"
           (is
            (= (dnf (negation constant-false))
               constant-true)))
  (testing "!0 = 1"
           (is
            (= (dnf (negation constant-true))
               constant-false))))

(deftest test-implication
  (testing "x -> y = !x + y"
           (is
            (= (dnf (implication (variable :x) (variable :y)))
               (disjunction (negation (variable :x)) (variable :y)))))
  (testing "x -> x = !x + x = 1"
           (is
            (= (dnf (implication (variable :x) (variable :x)))
               constant-true)))
  (testing "((x -> y) -> z = !x * !y + z"
           (is
            (= (dnf (implication (disjunction (variable :x) (variable :y)) (variable :z)))
               (disjunction (conjunction (negation (variable :x)) (negation (variable :y))) (variable :z)))))
  (testing "x -> y + z -> a = !x + y + !z + a"
           (is
            (=
             (dnf
              (disjunction (implication (variable :x) (variable :y))
                           (implication (variable :z) (variable :a))))
             (disjunction (negation (variable :x)) (variable :y) (negation (variable :z)) (variable :a)))))
  (testing "(x -> y) & (z & a) = (!x + y) * (!z + a) = !x * !z + y * !z + !x * a + !y * a"
           (is
            (=
             (dnf
              (conjunction (implication (variable :x) (variable :y))
                           (implication (variable :z) (variable :a))))
             (disjunction
              (conjunction (negation (variable :x)) (negation (variable :z)))
              (conjunction (variable :y) (negation (variable :z)))
              (conjunction (negation (variable :x)) (variable :a))
              (conjunction (variable :y) (variable :a)))))))

(deftest test-distributivity
  (testing "x * (y + z) = (x * y) + (x * z)"
           (is
            (= (dnf (conjunction (variable :x) (disjunction (variable :y) (variable :z))))
               (disjunction (conjunction (variable :x) (variable :y))
                            (conjunction (variable :x) (variable :z))))))
  (testing "(y + z) * x = (y * x) + (z * x)"
           (is
            (= (dnf (conjunction (disjunction (variable :y) (variable :z)) (variable :x)))
               (disjunction (conjunction (variable :y) (variable :x))
                            (conjunction (variable :z) (variable :x)))))))

(deftest initialized-expression-tests
  (testing "!x = 0, (x = 1)"
           (is
            (= (dnf-initialized (negation (variable :x)) (variable :x) constant-true)
               constant-false)))
  (testing "x * y = y, (x = 1)"
           (is
            (=
             (dnf-initialized (conjunction (variable :x) (variable :y)) (variable :x) constant-true)
             (variable :y))))
  (testing "x + y = 1, (x = 1)"
           (is
            (=
             (dnf-initialized (disjunction (variable :x) (variable :y)) (variable :x) constant-true)
             constant-true))))
