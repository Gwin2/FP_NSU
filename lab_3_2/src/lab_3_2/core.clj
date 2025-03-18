(ns lab-3-2.core
  (:gen-class))

(defn heavy-even? [x] (do (Thread/sleep 10) (even? x)))

(defn split-seq
  [coll chunk-sizes]
  (let [chunk-size (first chunk-sizes)
        split-coll (split-at chunk-size coll)]
    (lazy-seq
      (cons (first split-coll)
            (split-seq (second split-coll)
                       (rest chunk-sizes))))))

(defn partition
  [chunk-count coll]
  (let [coll-size  (count coll)
        chunk-size (quot coll-size chunk-count)
        remainder  (rem coll-size chunk-count)]
    (->> (range)
         (map
          #(if (< % remainder)
            (inc chunk-size)
            chunk-size))
         (split-seq coll)
         (take chunk-count))))

(defn lazy-parallel-filter
  [pred step n coll]
  (->> (partition (* step n) coll)
       (map
        (fn [list]
          (->> (partition step list)
               (map (fn [x] (future (doall (filter pred x)))))
               (doall)
               (map deref))))
       (flatten)))

(defn -main
  []
  (let [coll (take 500 (iterate inc 0))]
    (println "Sequence:")
    (println coll)
    (println "Standart filter:")
    (time (println (doall (filter heavy-even? coll))))
    (println "Lazy parallel filter:")
    (time (println (doall (lazy-parallel-filter heavy-even? 2 3 coll))))
    (shutdown-agents)))
