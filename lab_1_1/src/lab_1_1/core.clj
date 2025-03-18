(ns lab-1-1.core
  (:gen-class))

(defn generate-word [word alphabet]
  (when (not-empty alphabet)
    (let [recur-result (generate-word word
                                    (rest alphabet))]
      (if (not= (first alphabet) (first word))
        (conj recur-result
              (conj word (first alphabet)))
        recur-result))))

(defn generate-words [words alphabet]
  (when (not-empty words)
    (concat (generate-words (rest words) alphabet)
            (generate-word (first words) alphabet))))

(defn generate-permutations [n alphabet]
  (if (> n 0)
    (generate-words (generate-permutations (dec n) alphabet)
                  alphabet)
    '(())))

(defn -main []
  (println(generate-permutations 2 '(\a \b \c)))
  (println(generate-permutations 1 '("a" "b" "c")))
  (println(generate-permutations 2 '(:a [:b (:c)])))
  (println(generate-permutations 0 '(:a [:b (:c)])))
  )
