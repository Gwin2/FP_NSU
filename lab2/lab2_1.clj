(ns lab2_1)

(def delta 1/100)

(defn trapezium [f t_1 t_2]
  (* delta (/ (+ (f t_1) (f t_2)) 2)))

(defn rec-integral [f x_1 x_2 rec-fn]
  (if (>= x_1 x_2)
    0
    (+ (rec-fn f x_1 (- x_2 delta) rec-fn) (trapezium f (- x_2 delta) x_2))))

(def rec-integral-mem (memoize rec-integral))

(defn integral-mem [f]
  (fn [x] (rec-integral-mem f 0 x rec-integral-mem)))
(defn integral [f]
  (fn [x] (rec-integral f 0 x rec-integral)))

(def def-integral-mem (integral-mem #(* 3 %)))
(def def-integral (integral #(* -3 %)))

(defn -main []
  (time (def-integral 10))
  (time (def-integral 10))
  (time (def-integral 11))
  (time (def-integral-mem 10))
  (time (def-integral-mem 10))
  (time (def-integral-mem 9))
  (time (def-integral-mem 11)))

(-main) 