(ns task3.src.task3.core
  :require [clojure.test :refer :all])
(defn prepare-chunks
  "Create batch of chunks for use in futures"
  [s] (let [
            core-number (.availableProcessors (Runtime/getRuntime))
            chunk-size 10]
        (partition-all core-number (partition-all chunk-size s))))

; ->> - pass val through funcs

(defn convert-to-future
  "Pushes chunk to future and uses filter predicate"
  [pred s] (map #(->> %
                      (map
                        (fn [g]
                          (->> g
                               (filter pred)
                               (doall)
                               (future)))))
                s))



(defn convert-from-future
  "Derefs futures to get values"
  [s] (->> s
           (doall)
           (map deref)
           (reduce concat)))

(defn pfilter
  ([pred s] (pfilter (convert-to-future pred (prepare-chunks s))))
  ([s]
   (if (empty? s)
     (-> ())
     (lazy-seq (concat (convert-from-future (first s))
                       (pfilter (rest s)))))))

(defn slowFunc
  "'Slowly working' filter func, to see the optimisation of separation"
  [val]
  (Thread/sleep 9)
  (= (mod val 4) 0))

(deftest emptyTest (is (= (filter odd? (range -1)) (filter odd? (range 0)))))
(deftest valTest (is (= (filter odd? (range -1 1000)) (filter odd? (range 0 1000)))))

(deftest timetest (time (doall (take 99 (filter slowFunc (iterate inc 0)))))
         (time (doall (take 99 (pfilter slowFunc (iterate inc 0))))))
