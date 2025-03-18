(ns lab1_1
  (:require [clojure.string :as s]))

(defn add-char-to-string [chars string]
  (cond
    (= (count chars) 0) '()
    (> (count chars) 0) (if (s/ends-with? string (first chars))
                            (add-char-to-string (rest chars) string)
                            (cons
                             (str string (first chars))
                             (add-char-to-string (rest chars) string)))))

(defn add-char  [chars strings]
  (cond
    (= (count strings) 0) '()
    (> (count strings) 0) (concat
                         (add-char-to-string chars (first strings))
                         (add-char chars (rest strings)))))

(defn make-combinations [chars n]
  (cond
    (<= n 0) '()
    ;; (= n 1) chars
    (= (count chars) 0) '()
    (and (= (count chars) 1) (> n 1)) '()
    :else (add-char chars (make-combinations chars (dec n)))))

(defn -main []
  (println (make-combinations '() 3))
  (println (make-combinations '("a") 1))
  (println (make-combinations '("a") 3))
  (println (make-combinations '("a" "b" "c") -3))
  (println (make-combinations '("a" "b" "c") 2))
  (println (make-combinations '("a" "b" "c") 3)))

(-main)
