(ns μml.compile-test
  (:require [acolfut.sweet :refer :all]
            [μml.ast :refer :all]
            [μml.machine :refer :all]
            [μml.compile :refer :all])
  (:refer-clojure :exclude [pop compile]))

(deftest compile-test
  (testing "test compile"
    (let [expr1 (Fun "eq3" "x" IntT BoolT (Equal (Var "x") (Int 3)))
          expr2 (Apply expr1 (Int 3))
          expr3 (Plus (Times (Int 3) (Int 1)) (Int 2))]
      (is= (compile expr1)
           [(ClosureI "eq3" "x" [(VarI "x") (IntI 3) EqualI PopEnvI])])
      (is= (compile expr2)
           [(ClosureI "eq3" "x" [(VarI "x") (IntI 3) EqualI PopEnvI])
            (IntI 3)
            CallI])
      (is= (compile expr3)
           [(IntI 3) (IntI 1) MultI (IntI 2) AddI]))))
