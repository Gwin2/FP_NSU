(ns lab-4.dnf-rules
  (:require
    [lab-4.operations :refer :all]))

(declare simplify-complex-operations)
(declare simplify-negation)
(declare simplify-distributivity)
(declare simplify-inner-binary-operations)
(declare simplify-binary-operations-with-constant)

;Применяет к логическому выражению (expr) набор правил для упрощения (rules)
(defn apply-rules [expr rules]
  ((some
    (fn [rule]
      (if ((first rule) expr)
        (second rule)
        false))
    rules)
    expr))

;Правила упрощения сложных операций-------------------------------------------------------------------------------------
(def complex-operations-rules
  (list
   ;x -> y = !x + y
   [(fn [expr] (implication? expr))
    (fn [expr]
      (simplify-complex-operations
       (disjunction
        (negation (simplify-complex-operations (first (operation-args expr))))
        (simplify-complex-operations (second (operation-args expr))))))]
   ;(logical expr) * (logical expr) - операции, вложенные в коньюнкцию
   [(fn [expr] (conjunction? expr))
    (fn [expr]
      (apply conjunction (map simplify-complex-operations (operation-args expr))))]
   ;(logical expr) + (logical expr) - операции, вложенные в дизъюнкцию
   [(fn [expr] (disjunction? expr))
    (fn [expr]
      (apply disjunction (map simplify-complex-operations (operation-args expr))))]
   ;!(logical expr) - операция, к которой применили отрицание
   [(fn [expr] (negation? expr))
    (fn [expr] (negation (simplify-complex-operations (second expr))))]
   ;variable - переменная остается как есть
   [(fn [expr] (variable? expr))
    (fn [expr] expr)]
   ;constant - константа остается как есть
   [(fn [expr] (constant? expr))
    (fn [expr] expr)]))

(defn simplify-complex-operations [expr]
  (apply-rules expr complex-operations-rules))

;Правила упрощения отрицаний--------------------------------------------------------------------------------------------
(def negation-rules
  (list
   ;!(x*y) = !x + !y
   [(fn [expr] (and (negation? expr) (conjunction? (second expr))))
    (fn [expr]
      (simplify-negation
       (apply disjunction (map #(negation %) (operation-args (second expr))))))]
   ;!(x+y) = !x * !y
   [(fn [expr] (and (negation? expr) (disjunction? (second expr))))
    (fn [expr]
      (simplify-negation
       (apply conjunction (map #(negation %) (operation-args (second expr))))))]
   ;!!x = x
   [(fn [expr] (and (negation? expr) (negation? (second expr))))
    (fn [expr] (simplify-negation (first (operation-args (second expr)))))]
   ;!1 = 0
   [(fn [expr] (and (negation? expr) (constant-true? (first (operation-args expr)))))
    (fn [expr] constant-false)]
   ;!0 = 1
   [(fn [expr] (and (negation? expr) (constant-false? (first (operation-args expr)))))
    (fn [expr] constant-true)]
   ;(logical expr) * (logical expr) - операции, вложенные в коньюнкцию
   [(fn [expr] (conjunction? expr))
    (fn [expr] (apply conjunction (map simplify-negation (operation-args expr))))]
   ;(logical expr) + (logical expr) - операции, вложенные в дизъюнкцию
   [(fn [expr] (disjunction? expr))
    (fn [expr] (apply disjunction (map simplify-negation (operation-args expr))))]
   ;!(logical expr) - операция, к которой применили отрицание
   [(fn [expr] (negation? expr))
    (fn [expr] (negation (simplify-negation (second expr))))]
   ;variable - переменная остается как есть
   [(fn [expr] (variable? expr))
    (fn [expr] expr)]
   ;constant - константа остается как есть
   [(fn [expr] (constant? expr))
    (fn [expr] expr)]))

(defn simplify-negation [expr]
  (apply-rules expr negation-rules))

;Правила упрощения с использование дистрибутивности---------------------------------------------------------------------
(def distributivity-rules
  (list
   ; x * (y + z) = (x * y) + (x * z)
   [(fn [expr] (and (conjunction? expr) (disjunction? (nth expr 2))))
    (fn [expr]
      (simplify-distributivity
       (disjunction
        (conjunction
         (first (operation-args expr))
         (first (operation-args (second (operation-args expr)))))
        (conjunction
         (first (operation-args expr))
         (second (operation-args (second (operation-args expr))))))))]
   ; (y + z) * x = (y * z) + (z * x)
   [(fn [expr] (and (conjunction? expr) (disjunction? (second expr))))
    (fn [expr]
      (simplify-distributivity
       (disjunction
        (conjunction (first (operation-args (first (operation-args expr))))
                     (second (operation-args expr)))
        (conjunction (second (operation-args (first (operation-args expr))))
                     (second (operation-args expr))))))]
   ;(logical expr) * (logical expr) - операции, вложенные в коньюнкцию
   [(fn [expr] (conjunction? expr))
    (fn [expr] (apply conjunction (map simplify-distributivity (operation-args expr))))]
   ;(logical expr) + (logical expr) - операции, вложенные в дизъюнкцию
   [(fn [expr] (disjunction? expr))
    (fn [expr] (apply disjunction (map simplify-distributivity (operation-args expr))))]
   ;!(logical expr) - операция, к которой применили отрицание
   [(fn [expr] (negation? expr))
    (fn [expr] (negation (simplify-distributivity (second expr))))]
   ;variable - переменная остается как есть
   [(fn [expr] (variable? expr))
    (fn [expr] expr)]
   ;constant - константа остается как есть
   [(fn [expr] (constant? expr))
    (fn [expr] expr)]))

(defn simplify-distributivity
  [expr]
  (apply-rules expr distributivity-rules))

;Правила упрощения вложенных операций (аналог раскрытия скобок)---------------------------------------------------------
(def inner-binary-operations-rules
  (list
   ;Вложенные коньюнкции
   [(fn [expr] (and (conjunction? expr) (some conjunction? (operation-args expr))))
    (fn [expr]
      (let [conj      #(some (if (conjunction? %) % nil) (operation-args expr))
            conj-args (operation-args conj)]
        (simplify-inner-binary-operations
         (apply conjunction
                (concat (remove #(= % conj) (operation-args expr))
                        conj-args)))))]
   ;Вложенные дизъюнкции
   [(fn [expr] (and (disjunction? expr) (some disjunction? (operation-args expr))))
    (fn [expr]
      (let [disj      (some #(if (disjunction? %) % nil) (operation-args expr))
            disj-args (operation-args disj)]
        (simplify-inner-binary-operations
         (apply disjunction
                (concat (remove #(= % disj) (operation-args expr)) disj-args)))))]
   ;(logical expr) * (logical expr) - операции, вложенные в коньюнкцию
   [(fn [expr] (conjunction? expr))
    (fn [expr]
      (apply conjunction (map simplify-inner-binary-operations (operation-args expr))))]
   ;(logical expr) + (logical expr) - операции, вложенные в дизъюнкцию
   [(fn [expr] (disjunction? expr))
    (fn [expr]
      (apply disjunction (map simplify-inner-binary-operations (operation-args expr))))]
   ;!(logical expr) - операция, к которой применили отрицание
   [(fn [expr] (negation? expr))
    (fn [expr] (negation (simplify-inner-binary-operations (second expr))))]
   ;variable - переменная остается как есть
   [(fn [expr] (variable? expr))
    (fn [expr] expr)]
   ;constant - константа остается как есть
   [(fn [expr] (constant? expr))
    (fn [expr] expr)]))

(defn simplify-inner-binary-operations
  [expr]
  (apply-rules expr inner-binary-operations-rules))

;Упрощение операций, которые выполняются с использованием констант------------------------------------------------------
(def binary-operations-with-constant-rules
  (list
   ;Рассматривается общий случай. Если останется бинарная опреация с одним аргументом, то она удаляется псле
   ; x * 1 * (logical_expr) = x * (logical_expr)
   ; x * 0 * (logical_expr) = 0
   [(fn [expr] (and (conjunction? expr) (some constant? (operation-args expr))))
    (fn [expr]
      (if (=
           (some #(when (constant? %) %) (operation-args expr))
           constant-false)
        constant-false
        (apply conjunction (remove #(= % constant-true) (operation-args expr)))))]
   ; x + 1 + (logical_expr) = 1
   ; x + 0 + (logical_expr) = x + (logical_expr)
   [(fn [expr] (and (disjunction? expr) (some constant? (operation-args expr))))
    (fn [expr]
      (if (=
           (some #(when (constant? %) %) (operation-args expr))
           constant-true)
        constant-true
        (apply disjunction
               (map simplify-binary-operations-with-constant
                    (remove #(= % constant-false) (operation-args expr))))))]
   [(fn [expr] (conjunction? expr))
    (fn [expr] expr)]
   [(fn [expr] (disjunction? expr))
    (fn [expr]
      (apply disjunction
             (map simplify-binary-operations-with-constant (operation-args expr))))]
   [(fn [expr] (variable? expr))
    (fn [expr] expr)]
   [(fn [expr] (and (negation? expr) (variable? (first (operation-args expr)))))
    (fn [expr] expr)]
   [(fn [expr] (constant? expr))
    (fn [expr] expr)]))

(defn simplify-binary-operations-with-constant [expr]
  (apply-rules expr binary-operations-with-constant-rules))

;Упрощение бинарных операций с одним аргументом, если такие остались после раскрытия вложенных операций коньюнкции и дизьюнкции
(defn simplify-single-disjunction
  [expr]
  (if (disjunction? expr)
    (if (> (count (operation-args expr)) 1)
      expr
      (second expr))
    expr))

(defn simplify-single-conjunction
  [expr]
  (if (conjunction? expr)
    (if (> (count (operation-args expr)) 1)
      expr
      (second expr))
    expr))

;Процедуры очистки выражения от тафтологий и противоречий вида: !x * x = 0, !x + x = 1----------------------------------

;Проверяет наличие констант или переменных с отрицанием и возвращает в этом случае аргумент отрицания, иначе имя перменной или константу
(defn check-negation-variable-or-constant [x]
  {:pre [(or (variable? x) (constant? x)
             (and (negation? x)
                  (or (variable? (first (operation-args x))) (constant? (first (operation-args x))))))]}
  (if (or (variable? x) (constant? x))
    x
    (first (operation-args x))))

(defn simplify-contradictions-conjunction [expr]
  (let [args-list         (operation-args expr)
        no-negations-list (map (fn [elem] (check-negation-variable-or-constant elem)) args-list)]
    (if (< (count (distinct no-negations-list)) (count (distinct args-list)))
      constant-false
      expr)))

(defn simplify-conjunction [expr]
  (let [result (if (or (constant? expr) (variable? expr)
                       (and (negation? expr) (variable? (first (operation-args expr)))))
                 expr
                 (simplify-contradictions-conjunction
                  (apply conjunction (distinct (operation-args expr)))))]
    (simplify-single-conjunction result)))

(defn simplify-contradictions-disjunction [expr]
  (let [args-list         (operation-args expr)
        no-negations-list (map #(if (negation? %) (second %) %) args-list)]
    (if (< (count (distinct no-negations-list)) (count (distinct args-list)))
      constant-true
      (distinct expr))))

(defn simplify-disjunction [expr]
  (->>
   (apply disjunction
          (map simplify-conjunction
               (if  (or (constant? expr) (variable? expr)
                        (and (negation? expr) (variable? (first (operation-args expr)))))
                 expr
                 (operation-args expr))))
   simplify-contradictions-disjunction))

(defn simplify
  ;1. если expr - константа или переменная, ничего не делать;
  ;2. если expr - конъюнкция, то упростить конъюнкцию, убрать дубликаты и противоречия;
  ;3. если expr - дизюнкция, то упростить конъюнкции в составе, убрать дубликаты и тафтологии
  ;4. если осталась дизъюнкция с одним аргументом, то упростить"
  [expr]
  (let [result (cond
                 (constant? expr)    expr
                 (variable? expr)    expr
                 (negation? expr)    expr
                 (conjunction? expr) (simplify-conjunction expr)
                 (disjunction? expr) (simplify-disjunction expr))]
    (simplify-single-disjunction result)))

;Преобразует лоигческое выражение в ДФН согласно правилам логических операций упрощения
(defn dnf [expr]
  (->>
   expr
   ; 1. Избавиться от всех логических операций, содержащихся в формуле, заменив их основными: конъюнкцией, дизъюнкцией, отрицанием.
   simplify-complex-operations
   ; 2. Заменить знак отрицания, относящийся ко всему выражению, знаками отрицания, относящимися к отдельным переменным высказываниям на основании формул:
   ; !(x * y) = !x + !y
   ; !(x + y) = !x * !y
   ; 3. Избавиться от знаков двойного отрицания.
   simplify-negation
   ; 4. Применить, если нужно, к операциям конъюнкции и дизъюнкции свойства дистрибутивности и убрать вложенность.
   simplify-distributivity
   simplify-inner-binary-operations
   simplify
   ; убрать случаи по типу x * 1 * z = x * z
   simplify-binary-operations-with-constant
   simplify))