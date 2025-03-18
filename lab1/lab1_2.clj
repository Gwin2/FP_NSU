(ns lab1_2
  (:require [clojure.string :as s]))

(defn add-char-to-string-recur 
  ([chars string]
   (add-char-to-string-recur chars string '()))
  ([chars string res]
  (cond
    (= (count chars) 0) res
    (> (count chars) 0) (if (s/ends-with? string (first chars))
                          (recur (rest chars) string res)
                          (recur (rest chars) 
                                 string 
                                 (concat res (list (str string (first chars)))))))))

(defn add-char-recur 
  ([chars strings]
   (add-char-recur chars strings '()))
  ([chars strings res] 
   (cond
     (= (count strings) 0) res
     (> (count strings) 0) (recur chars
                                  (rest strings)
                                  (concat res (add-char-to-string-recur chars (first strings)))))))

(defn make-combinations-recur 
  ([chars n] 
   (cond 
     (<= n 0) '() 
     (= n 1) chars 
     (= (count chars) 0) '() 
     (and (= (count chars) 1) (> n 1)) '() 
     :else (make-combinations-recur chars (dec n) chars)))
  ([chars n res]
   (if (= n 0)
     res
     (recur chars (dec n) (add-char-recur chars res)))))

(make-combinations-recur '("a" "b" "c") 3)

(defn -main []
  (println (make-combinations-recur '() 3))
  (println (make-combinations-recur '("a") 1))
  (println (make-combinations-recur '("a") 3))
  (println (make-combinations-recur '("a" "b" "c") -3))
  (println (make-combinations-recur '("a" "b" "c") 2))
  (println (make-combinations-recur '("a" "b" "c") 3)))

(-main)
