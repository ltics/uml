(ns μml.type-check-test
  (require [acolfut.sweet :refer :all]
           [μml.ast :refer :all]
           [μml.type-check :refer :all]))

(deftest type-check-test
  (testing "type check"
    (let [ctx {"a" IntT
               "b" BoolT}
          expr1 (Var "a")
          expr2 (Var "b")
          expr3 (Var "c")
          expr4 (Times (Int 3) (Var "a"))
          expr5 (Plus (Int 3) (Var "b"))
          expr6 (If (Bool true) (Int 3) (Var "a"))
          expr7 (If (Var "a") (Int 3) (Var "a"))
          expr8 (If (Var "b") (Int 3) (Var "b"))
          expr9 (Fun "add" "x" IntT BoolT (Equal (Var "x") (Int 3)))
          expr10 (Fun "add" "x" IntT IntT (Equal (Var "x") (Int 3)))
          expr11 (Apply expr9 (Var "a"))
          expr12 (Apply expr9 (Var "b"))]
      (is (check ctx IntT expr1))
      (is (thrown? Exception (check ctx IntT expr2)))
      (is (thrown? Exception (check ctx IntT expr3)))
      (is (check ctx IntT expr4))
      (is (thrown? Exception (check ctx IntT expr5)))
      (is (check ctx IntT expr6))
      (is (thrown? Exception (check ctx IntT expr7)))
      (is (thrown? Exception (check ctx IntT expr8)))
      (is= (type-of ctx expr9) (ArrowT IntT BoolT))
      (is (check ctx (ArrowT IntT BoolT) expr9))
      (is (thrown? Exception (check ctx (ArrowT IntT IntT) expr10)))
      (is (check ctx BoolT expr11))
      (is (thrown? Exception (check ctx BoolT expr12))))))
