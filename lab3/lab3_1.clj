(ns lab3_1)

(def threads-number (.availableProcessors (Runtime/getRuntime)))

(defn heavy-even? [x] (Thread/sleep 10) (even? x))

(defn my-partition [threads coll]
  (when (not-empty coll)
    (let [n (/ (count coll) threads)] 
      (cons 
       (take n coll) 
       (my-partition (dec threads) (drop n coll))))))

(defn pfilter [pred coll]
  (->> coll
       (my-partition threads-number)
       (map #(future (doall (filter pred %))))
       (doall)
       (mapcat deref)))

(defn -main []
  (time (doall (pfilter heavy-even? (range 100))))
  (time (doall (pfilter heavy-even? (range 100))))
  (time (doall (filter heavy-even? (range 100)))))

(-main)
