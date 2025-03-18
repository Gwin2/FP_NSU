(ns api_dnf
  (:use api_logic))

(defn impl-elimination
  "x -> y = -x | y"
  [expr]
  (cond
    (logic-or? expr) (apply logic-or (map #(impl-elimination %) (args expr)))
    (logic-and? expr) (apply logic-and (map #(impl-elimination %) (args expr)))
    (logic-not? expr) (logic-not (impl-elimination (first-arg expr)))
    (logic-impl? expr) (logic-or (logic-not (impl-elimination (first-arg expr))) (impl-elimination (second-arg expr)))
    :else expr))

(defn negation-equivalencies
  "-(x | y) = -x & -y; -(x & y) = -x | -y
   -(-x) = x
   -0 = 1; -1 = 0"
  [expr]
  (cond
    (logic-not? expr) (let [sub-expr (first-arg expr)]
                        (cond
                          (logic-or? sub-expr) (apply logic-and (map #(negation-equivalencies (logic-not %)) (args sub-expr)))
                          (logic-and? sub-expr) (apply logic-or (map #(negation-equivalencies (logic-not %)) (args sub-expr)))
                          (logic-not? sub-expr) (negation-equivalencies (first-arg sub-expr))
                          (constant? sub-expr) (if (= sub-expr (constant 0)) (constant 1) (constant 0))
                          :else expr))
    (logic-or? expr) (apply logic-or (map #(negation-equivalencies %) (args expr)))
    (logic-and? expr) (apply logic-and (map #(negation-equivalencies %) (args expr)))
    :else expr))

(defn absorption-law
  "x | (x & y) = x; x & (x | y) = x"
  ([expr]
   (cond
     (logic-or? expr) (absorption-law logic-and? logic-or (first-arg expr) (second-arg expr))
     (logic-and? expr) (absorption-law logic-or? logic-and (first-arg expr) (second-arg expr))
     :else expr))
  ([inner-op-check outer-op sub-expr1 sub-expr2]
   (cond
     (inner-op-check sub-expr1) (if (or (= sub-expr2 (first-arg sub-expr1)) (= sub-expr2 (second-arg sub-expr1)))
                                  (absorption-law sub-expr2)
                                  (outer-op (absorption-law sub-expr1) (absorption-law sub-expr2)))
     (inner-op-check sub-expr2) (if (or (= sub-expr1 (first-arg sub-expr2)) (= sub-expr1 (second-arg sub-expr2)))
                                  (absorption-law sub-expr1)
                                  (outer-op (absorption-law sub-expr1) (absorption-law sub-expr2)))
     :else (outer-op (absorption-law sub-expr1) (absorption-law sub-expr2)))))

(defn distributive-law
  "(x | y) & z = (x & z) | (y & z)"
  [expr]
  (cond
    (logic-and? expr) (let [sub-expr1 (first-arg expr) sub-expr2 (second-arg expr)]
                        (cond
                          (logic-or? sub-expr1) (logic-or (distributive-law (logic-and sub-expr2 (first-arg sub-expr1)))
                                                          (distributive-law (logic-and sub-expr2 (second-arg sub-expr1))))
                          (logic-or? sub-expr2) (logic-or (distributive-law (logic-and sub-expr1 (first-arg sub-expr2)))
                                                          (distributive-law(logic-and sub-expr1 (second-arg sub-expr2))))
                          :else expr))
    (logic-or? expr) (apply logic-or (map #(distributive-law %) (args expr)))
    :else expr))

(defn decompose
  "x & (y & z) = x & y & z"
  [expr]
  (if (or (logic-or? expr) (logic-and? expr))
    (cons (get-type expr) (map #(decompose %) (combine-same-args expr)))
    expr))

(decompose (logic-and (variable :x) (logic-and (variable :y) (variable :z))))

(defn idempotent-law
  "x & x = x; x | x = x"
  [expr]
  (cond
    (logic-and? expr) (apply logic-and (map #(idempotent-law %) (distinct (args (decompose expr)))))
    (logic-or? expr) (apply logic-or (map #(idempotent-law %) (distinct (args (decompose expr)))))
    (logic-not? expr) (logic-not (idempotent-law (first-arg expr)))
    :else expr))

(defn identity-and-domination-laws
  "x & 0 = 0; x | 1 = 1
   x & 1 = x; x | 0 = x"
  ([expr]
   (let [expr (decompose expr)]
     (cond
       (logic-and? expr) (cond 
                           (identity-and-domination-laws expr (constant 0)) (constant 0)
                           (identity-and-domination-laws expr (constant 1)) (identity-and-domination-laws (apply logic-and (filter #(not (= % (constant 1))) (args expr))))
                           :else (apply logic-and (map #(identity-and-domination-laws %) (args expr))))
       (logic-or? expr) (cond
                          (identity-and-domination-laws expr (constant 1)) (constant 1)
                          (identity-and-domination-laws expr (constant 0)) (identity-and-domination-laws (apply logic-or (filter #(not (= % (constant 0))) (args expr))))
                          :else (apply logic-or (map #(identity-and-domination-laws %) (args expr))))
       :else expr)))
  ([expr const]
   (some #(and (constant? %) (= const %)) (args expr))))

(defn dnf
  "Build DNF"
  [expr]
  (->> expr
       (impl-elimination)
       (negation-equivalencies)
       (distributive-law)
       (absorption-law)
       (idempotent-law)
       (identity-and-domination-laws)))

(defn substitution
  "Substitute variables into expression"
  [expr var-map]
  (let [expr (identity-and-domination-laws expr)] 
    (cond 
      (variable? expr) (let [var-name (first-arg expr)] 
                         (if (contains? var-map var-name) 
                           (constant (get var-map var-name)) 
                           expr)) 
      (constant? expr) expr 
      :else (cons (get-type expr) (map #(substitution % var-map) (args expr))))))