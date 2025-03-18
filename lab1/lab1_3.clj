(ns lab1_3)

(defn my-map-old [f coll] 
  (reduce 
   (fn [coll x] (concat coll (list (f x)))) 
   '() coll))

(defn my-map-recur [f coll]
  (if (= (count coll) 0)
    '()
    (concat (list (f (first coll))) (my-map-recur f (rest coll)))))

(defn my-map [f coll]
  (reduce #(conj %1 (f %2)) 
                      [] coll))

(def arr (repeat 10000 2))
(time (my-map inc arr))
(time (my-map-old inc arr))

(my-map inc '(1 2 3))
(my-map (fn [n] (* n 3)) '(1 2 3))
(my-map char '(97 98 99))
(my-map int '(\a \b \c))

(my-map-recur inc '(1 2 3))
(my-map-recur (fn [n] (* n 3)) '(1 2 3))
(my-map-recur char '(97 98 99))
(my-map-recur int '(\a \b \c))

;; _________________________________________________________________

(defn my-filter-old [pred coll]
  (reduce
   (fn [coll x] (if (pred x) 
                  (concat coll (list x)) 
                  coll)) 
   '() coll))

(defn my-filter-recur [pred coll]
  (if (= (count coll) 0)
    '()
    (if (pred (first coll))
      (concat (list (first coll)) (my-filter-recur pred (rest coll)))
      (my-filter-recur pred (rest coll)))))

(defn my-filter [pred coll]
  (apply list (reduce #(if (pred %2) 
                         (conj %1 %2) 
                         %1) 
                      [] coll)))

(my-filter even? '(1 2 3 4))
(my-filter odd? '(1 2 3 4))
(my-filter pos? '(-1 0 1 -2 3 -4 6))

(my-filter-recur even? '(1 2 3 4))
(my-filter-recur odd? '(1 2 3 4))
(my-filter-recur pos? '(-1 0 1 -2 3 -4 6))
