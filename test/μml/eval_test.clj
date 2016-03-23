(ns μml.eval-test
  (require [acolfut.sweet :refer :all]
           [μml.ast :refer :all]
           [μml.eval :refer :all])
  (:refer-clojure :exclude [eval]))

(deftest eval-test
  (testing "test eval1"
    (let [expr1 (Times (Times (Int 3) (Int 3)) (Int 3))
          expr2 (Less (Times (Int 3) (Int 3)) (Int 33))
          expr3 (If (Bool false) (Int 3) (Int 33))
          expr4 (If expr2 (Int 3) (Int 33))
          incfn (Fun "inc" "x" IntT IntT (Plus (Var "x") (Int 1)))
          expr5 (Apply incfn (Int 2))
          expr6 (Apply incfn expr1)]
      (is= (eval1 expr1) (Times (Int 9) (Int 3)))
      (is= (eval1 expr2) (Less (Int 9) (Int 33)))
      (is= (eval1 expr3) (Int 33))
      (is= (eval1 expr4) (If (Less (Int 9) (Int 33)) (Int 3) (Int 33)))
      (is= (eval1 expr5) (Plus (Int 2) (Int 1)))
      (is= (eval1 expr6) (Apply incfn (Times (Int 9) (Int 3))))
      (is (thrown? Exception (eval1 incfn)))))
  (testing "test eval"
    (let [expr1 (Times (Times (Int 3) (Int 3)) (Int 3))
          expr2 (Less (Times (Int 3) (Int 3)) (Int 33))
          expr3 (If (Bool false) (Int 3) (Int 33))
          expr4 (If expr2 (Int 3) (Int 33))
          incfn (Fun "inc" "x" IntT IntT (Plus (Var "x") (Int 1)))
          expr5 (Apply incfn (Int 2))
          expr6 (Apply incfn expr1)]
      (is= (eval expr1) (Int 27))
      (is= (eval expr2) (Bool true))
      (is= (eval expr3) (Int 33))
      (is= (eval expr4) (Int 3))
      (is= (eval expr5) (Int 3))
      (is= (eval expr6) (Int 28)))))
