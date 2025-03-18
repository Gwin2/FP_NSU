(ns lab-2-2.core)

(defn heavy-function [x] (do (Thread/sleep 10) (* 2 x)))

;Additional function----------------------------------------------------

(defn trapezia-area [func x y]
  (* (/ (+ (func x) (func y)) 2) (- y x)))

;Sequence functions-----------------------------------------------------

(defn calc-patitial-sum-seq [function step]
  (reductions + 0 (map #(trapezia-area function % (+ step %)) (iterate #(+ step %) 0))))

(defn calc-integral-seq
  [function step]
  (let [integrate-seq (calc-patitial-sum-seq function step)]
    (fn [x]
      (nth integrate-seq (quot x step)))))

(defn -main
  []
  (println "Sequenced way")
  (let [integrate-mem (calc-integral-seq heavy-function 0.5)]
    (time (integrate-mem 100))
    (time (integrate-mem 99))
    (time (integrate-mem 101))
    (time (integrate-mem 100))))
