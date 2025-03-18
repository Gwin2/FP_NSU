(ns lab-4.operations)

;Описание значений констант
(def constant-true (list :true))
(def constant-false (list :false))

;Проверка, что значение - истина
(defn constant-false? [expr]
  (= (first expr) :false))

;Проверка, что значение - ложь
(defn constant-true? [expr]
  (= (first expr) :true))

;Описание константы
(defn constant [expr]
  (cond
    (constant-true? expr)  constant-true
    (constant-false? expr) constant-false))

;Проверка, что выражение является константой
(defn constant? [expr]
  (or (constant-true? expr)
      (constant-false? expr)))

;Описание переменной
(defn variable [name]
  {:pre [(keyword? name)]}
  (list :var name))

;Описание имени переменной
(defn variable-name [v]
  (second v))

;Проверка, что выражение является переменной
(defn variable? [expr]
  (= (first expr) :var))

;Проверка равенства переменных (нужно для удаления дублей)
(defn same-variables? [v1 v2]
  (and
   (variable? v1)
   (variable? v2)
   (= (variable-name v1)
      (variable-name v2))))

;Описание коньюнкции
(defn conjunction
  [expr & rest]
  (cons :conjuction (cons expr rest)))

;Проверка на коньюнкцию
(defn conjunction? [expr]
  (= (first expr) :conjuction))

;Описание дизъюнкции
(defn disjunction
  [expr & rest]
  (cons :disjunction (cons expr rest)))

;Проверка на дизъюнкцию
(defn disjunction? [expr]
  (= (first expr) :disjunction))

;Описание отрицания
(defn negation
  [expr & rest]
  (cons :negation (cons expr rest)))

;Проверка на отрицание
(defn negation? [expr]
  (= (first expr) :negation))

;Описание импликации
(defn implication
  [expr & rest]
  (cons :implication (cons expr rest)))

;Проверка на импликацию
(defn implication? [expr]
  (= (first expr) :implication))

;Аргументы операции
(defn operation-args [expr]
  (rest expr))