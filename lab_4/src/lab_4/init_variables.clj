(ns lab-4.init-variables
  (:require
    [lab-4.operations :refer :all]
    [lab-4.dnf-rules :refer :all]))

(declare init-expression-variables)

(defn init-rules [var value]
  (list
   [(fn [expr] (and (variable? expr) (same-variables? var expr)))
    (fn [expr] value)]
   [(fn [expr] (conjunction? expr))
    (fn [expr]
      (apply conjunction
             (map #(init-expression-variables var value %) (operation-args expr))))]
   [(fn [expr] (disjunction? expr))
    (fn [expr]
      (apply disjunction
             (map #(init-expression-variables var value %) (operation-args expr))))]
   [(fn [expr] (negation? expr))
    (fn [expr] (negation (init-expression-variables var value (second expr))))]
   [(fn [expr] (or (variable? expr) (constant? expr)))
    (fn [expr]
      expr)]))

(defn init-expression-variables [var val expr]
  (apply-rules expr (init-rules var val)))

(defn dnf-initialized [expr var val]
  (->>
   expr
   dnf
   (init-expression-variables var val)
   dnf))