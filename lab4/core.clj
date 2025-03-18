(ns core
  (:use api_logic) 
  (:use api_dnf))

(defn -main []
  (println "x -> y =" (expr-to-str (dnf (logic-impl (variable :x) (variable :y)))))
  (println "1 -> y =" (expr-to-str (dnf (logic-impl (constant 1) (variable :y)))))
  (println "1 -> (x -> y) =" (expr-to-str (dnf (logic-impl (constant 1) (logic-impl (variable :x) (variable :y))))))
  (println "x | y | z =" (expr-to-str (dnf (logic-or (variable :x) (logic-or (variable :y) (variable :z))))))
  (println "-((x -> y) | -(y -> z)) =" (expr-to-str (dnf (logic-not (logic-or
                                                                         (logic-impl (variable :x) (variable :y))
                                                                         (logic-not (logic-impl (variable :y) (variable :z)))))))))


(-main)