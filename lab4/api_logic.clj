(ns api_logic)

(defn constant 
  "Constructor for constant" 
  [value] 
  {:pre [(or (= value 0) (= value 1))]}
  (list ::const value))

(defn constant? 
  "Check if expr is constant" 
  [expr] 
  (= (first expr) ::const))

(defn constant-value 
  "Get constant value" 
  [expr] 
  (second expr))

(defn variable 
  "Constructor for variable" 
  [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? 
  "Check if expr is variable" 
  [expr] 
  (= (first expr) ::var))

(defn variable-name 
  "Get variable name" 
  [var]
  (name (second var)))

(defn same-variables? 
  "Check if two variable are the same" 
  [var1 var2]
  (and
   (variable? var1)
   (variable? var2)
   (= (variable-name var1)
      (variable-name var2))))

(defn logic-not
  "Constructor for negation"
  [expr]
  (list ::not expr))

(defn logic-not?
  "Check if expression is negation"
  [expr]
  (= (first expr) ::not))

(defn logic-or
  "Constructor for disjunction"
  [expr & rest]
  (if (empty? rest)
    expr
    (cons ::or (cons expr rest))))

(defn logic-or?
  "Check if expression is disjunction"
  [expr]
  (= (first expr) ::or))

(defn logic-and
  "Constructor for conjunction"
  [expr & rest]
  (if (empty? rest)
    expr
    (cons ::and (cons expr rest))))

(defn logic-and?
  "Check if expression is conjunction"
  [expr] 
  (= (first expr) ::and))

(defn logic-impl
  "Constructor for implication"
  [expr1 expr2] 
  (list ::impl expr1 expr2))

(defn logic-impl?
  "Check if expression is implication"
  [expr]
  (= (first expr) ::impl))

(defn get-type
  [expr]
  (keyword (first expr)))

(defn same-type?
  "Check that expr1 and expr2 are of the same type"
  [expr1 expr2]
  (= (first expr1) (first expr2)))

(defn args
  "Get arguments of operation"
  [expr]
  (rest expr))

(defn first-arg
  "Get arguments of operation"
  [expr]
  (first (rest expr)))

(defn second-arg
  "Get arguments of operation"
  [expr]
  (second (rest expr)))

(defn combine-same-args
  "Combine arguments of the same type in one operation"
  [expr]
  (if (or (constant? expr) (variable? expr))
    (list expr)
    (->> (args expr)
         (mapcat #(if (same-type? expr %)
                    (combine-same-args %)
                    (list %))))))

(defn expr-to-str
  "Converting expression to string"
  [expr]
  (cond
    (constant? expr) (constant-value expr)
    (variable? expr) (variable-name expr) 
    (logic-not? expr) (str "(" (str "-" (expr-to-str (first (args expr)))) ")")
    (logic-or? expr) (str "(" (reduce #(str %1 " | " %2) 
                                      (expr-to-str (first (args expr)))
                                      (map expr-to-str (rest (args expr)))) ")")
    (logic-and? expr) (str "(" (reduce #(str %1 " & " %2) 
                                       (expr-to-str (first (args expr)))
                                       (map expr-to-str (rest (args expr)))) ")")
    (logic-impl? expr) (str "(" (str (expr-to-str (first (args expr))) " -> " (expr-to-str (second (args expr)))) ")")))