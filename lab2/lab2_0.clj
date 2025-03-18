(ns lab2_0)

(def delta 1/10)

(defn trapezium [f t_1 t_2]
  (* delta (/ (+ (f t_1) (f t_2)) 2)))

(defn integral [f]
  (fn [x] (reduce + 0 (map #(trapezium f % (+ % delta)) (range 0 x delta)))))

(def def-integral (integral #(* 3 %)))

(defn -main []
  (time (def-integral 10))
  (time (def-integral 10))
  (time (def-integral 15)))

(-main)