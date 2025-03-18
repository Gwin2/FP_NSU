(ns lab1_4
  (:require [clojure.string :as s]))

(defn add-char-to-string [chars string]
  (map #(str string %1) (filter #(not (s/ends-with? string %1)) chars)))

(defn add-char [chars strings]
  (reduce concat (map #(add-char-to-string chars %1) strings)))

(defn make-combinations [chars n]
  (cond
    (<= n 0) '()
    (= (count chars) 0) '()
    (and (= (count chars) 1) (> n 1)) '()
    :else (nth (iterate #(add-char chars %1) chars) (dec n))))

(defn -main []
  (println (make-combinations '() 3))
  (println (make-combinations '("a") 1))
  (println (make-combinations '("a") 3))
  (println (make-combinations '("a" "b" "c") -3))
  (println (make-combinations '("a" "b" "c") 2))
  (println (make-combinations '("a" "b" "c") 3)))

(-main)
