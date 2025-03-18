(ns lab3_2)

(def threads-number (.availableProcessors (Runtime/getRuntime)))
(def slice-size 4)

(defn heavy-even? [x] (Thread/sleep 10) (even? x))

(defn my-partition [n coll]
  (when (not-empty coll)
    (lazy-seq (cons 
               (take n coll) 
               (my-partition n (drop n coll))))))

(defn lazy-pfilter [pred coll]
  (->> coll
       (my-partition (* threads-number slice-size))
       (map #(my-partition slice-size %)) 
       (mapcat (fn [x] (->> x 
                            (map #(future (doall (filter pred %)))) 
                            (doall))))
       (mapcat deref)))

(defn -main []
  (time (nth (lazy-pfilter heavy-even? (range)) 50))
  (time (nth (lazy-pfilter heavy-even? (range)) 50))
  (time (nth (filter heavy-even? (range)) 50)))

(-main)