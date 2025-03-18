(ns lab-1-3.core
  (:gen-class))

(defn my-reduce [fun init coll]
  ((fn [acc lst]
     (if (empty? lst)
       acc
       (recur (fun acc (first lst)) (rest lst))))
   init (seq coll)))

(defn my-map [f coll]
  (my-reduce (fn [acc elem] (conj acc (f elem))) [] coll))

(defn my-filter [pred coll]
  (my-reduce (fn [acc elem] (if (pred elem) (conj acc elem) acc)) [] coll))

(defn -main [& args]
  (println (my-map inc [1 2 3 4 5]))
  (println (my-filter even? (range 10))))