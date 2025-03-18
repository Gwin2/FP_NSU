(ns lab-3-1.core
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

(defn parallel-filter
  [pred n coll]
  (->> (partition n coll)
       (map #(future (doall (filter pred %))))
       (doall)
       (mapcat deref)))

(defn -main
  []
  (let [coll (take 500 (iterate inc 0))]
    (println "Sequence:")
    (println coll)
    (println "Standart filter:")
    (time (println (doall (filter heavy-even? coll))))
    (println "Parallel filter:")
    (time (println (doall (parallel-filter heavy-even? 3 coll))))
    (shutdown-agents)))
