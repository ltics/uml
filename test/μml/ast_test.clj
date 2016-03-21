(ns μml.ast-test
  (require [acolfut.sweet :refer :all]
           [μml.ast :refer :all]))

(deftest ast-test
  (testing "string of type"
    (let [type1 IntT
          type2 BoolT
          type3 (ArrowT IntT BoolT)
          type4 (ArrowT (ArrowT IntT BoolT) BoolT)
          type5 (ArrowT IntT (ArrowT IntT BoolT))]
      (is= (string-of-type type1) "int")
      (is= (string-of-type type2) "bool")
      (is= (string-of-type type3) "int -> bool")
      (is= (string-of-type type4) "(int -> bool) -> bool")
      (is= (string-of-type type5) "int -> int -> bool")))
  (testing "string of expr"
    (let [expr1 (Times (Int 1) (Int 3))
          expr2 (Fun "add" "x" IntT BoolT (Equal (Var "x") (Int 3)))
          expr3 (Apply (Fun "add" "x" IntT BoolT (Equal (Var "x") (Int 3))) (Int 1))]
      (is= (string-of-expr expr1) "(1 * 3)")
      (is= (string-of-expr expr2) "(fun add (x: int): bool is (x == 3))")
      (is= (string-of-expr expr3) "((fun add (x: int): bool is (x == 3)) 1)")))
  (testing "substitution free variable"
    (let [env {"x" (Int 3) "y" (Int 1) "add" (Bool true)}
          expr1 (Var "x")
          expr2 (Plus (Var "x") (Var "y"))
          expr3 (Fun "add" "x" IntT BoolT (Equal (Var "x") (Var "y")))]
      (is= (subst env expr1) (Int 3))
      (is= (subst env expr2) (Plus (Int 3) (Int 1)))
      (is= (subst env expr3) (Fun "add" "x" IntT BoolT (Equal (Var "x") (Int 1)))))))
