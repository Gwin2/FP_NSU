(ns lab-1-4.core
  (:gen-class))

(defn generate-word [word alphabet]
  (map #(conj word %1)
      (filter #(not= %1 (first word))
            alphabet)))

(defn generate-words [words alphabet]
  (reduce concat (map #(generate-word %1 alphabet) words)))

(defn generate-permutations [n alphabet]
  (nth (iterate #(generate-words %1 alphabet) '(())) n))

(defn -main []
  (println (generate-permutations 2 '(\a \b \c)))
  (println (generate-permutations 2 '("a" "b" "c")))
  (println (generate-permutations 2 '(:a [:b (:c)]))))