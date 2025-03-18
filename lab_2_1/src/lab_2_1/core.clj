(ns lab-2-1.core)

(defn heavy-function [x] (do (Thread/sleep 10) (* 2 x)))

;Additional function----------------------------------------------------

(defn trapezia-area [func x y]
  (* (/ (+ (func x) (func y)) 2) (- y x)))

;Memoize way--------------------------------------------------------------

(defn calc-integral-mem [function step]
  (let [integrate     (fn [integrate function step x]
                        (if (< 0 x)
                          (+
                           (trapezia-area function (- x step) x)
                           (integrate integrate function step (- x step)))
                          0))
        mem-integrate (memoize integrate)]
    (partial mem-integrate mem-integrate function step)))

(defn -main
  []
  (println "Memoized way")
  (let [integrate-mem (calc-integral-mem heavy-function 0.5)]
    (time (integrate-mem 100))
    (time (integrate-mem 99))
    (time (integrate-mem 101))
    (time (integrate-mem 100))))
