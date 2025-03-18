(ns lab2_2)

(def delta 1/10)

(defn trapezium [f t_1 t_2]
  (* delta (/ (+ (f t_1) (f t_2)) 2)))

(defn lazy-integral [f]
  (reductions + 0 (map #(trapezium f % (+ % delta)) (iterate #(+ % delta) 0))))

(defn get-integral [f]
  (fn [x] (nth (lazy-integral f) (/ x delta))))

(def def-integral (get-integral #(* 3 %)))

(defn -main []
  (time (def-integral 10))
  (time (def-integral 10))
  (time (def-integral 15)))

(-main)