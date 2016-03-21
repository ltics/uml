(ns μml.eval
  (require [adt.sweet :refer :all]
           [μml.ast :refer :all]))

(defn is-value
  [expr]
  (match expr
    (Int _) true
    (Bool _) true
    (Fun _) true
    :else false))

(defn eval1
  "single step evaluation"
  [expr]
  (if (is-value expr)
    (throw (Exception. "indicating a value"))
    (let [arith-bin-op (fn [expr-struct op e1 e2]
                         (match e1
                           (Int v1) (match e2
                                      (Int v2) (Int (op v1 v2))
                                      :else (expr-struct e1 (eval1 e2)))
                           :else (expr-struct (eval1 e1) e2)))
          cmp-bin-op   (fn [expr-struct op e1 e2]
                         (match e1
                           (Int v1) (match e2
                                      (Int v2) (Bool (op v1 v2))
                                      :else (expr-struct e1 (eval1 e2)))
                           :else (expr-struct (eval1 e1) e2)))]
      (match expr
        (Var _) (throw (Exception. "indicating a runtime error"))
        (Times e1 e2) (arith-bin-op Times * e1 e2)
        (Plus e1 e2) (arith-bin-op Plus + e1 e2)
        (Minus e1 e2) (arith-bin-op Minus - e1 e2)
        (Equal e1 e2) (cmp-bin-op Equal = e1 e2)
        (Less e1 e2) (cmp-bin-op Less < e1 e2)))))
