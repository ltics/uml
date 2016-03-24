(ns μml.parser-test
  (require [acolfut.sweet :refer :all]
           [μml.ast :refer :all]
           [μml.parser :refer :all]))

(deftest parser-test
  (testing "build ast"
    (is= (parse "(1 * 2)")
         (parse "1 * 2")
         [(Expr (Times (Int 1) (Int 2)))])
    (is= (parse "3 - 1")
         [(Expr (Minus (Int 3) (Int 1)))])
    (is= (parse "1 + 2 * 3")
         [(Expr (Plus (Int 1) (Times (Int 2) (Int 3))))])
    (is= (parse "(1 + 2) * 3")
         [(Expr (Times (Plus (Int 1) (Int 2)) (Int 3)))])
    (is= (parse "if true then 1 else 2")
         [(Expr (If (Bool true) (Int 1) (Int 2)))])
    (is= (parse "if (3 < 1) then 1 else 2")
         [(Expr (If (Less (Int 3) (Int 1)) (Int 1) (Int 2)))])
    (is= (parse "fun eq3 (x: int): bool is 3 = x")
         (parse "fun eq3 (x: int): bool is 3 = x;;")
         [(Expr (Fun "eq3" "x" IntT BoolT (Equal (Int 3) (Var "x"))))])
    (is= (parse "fun inc (x: int): int is 1 + x;;")
         [(Expr (Fun "inc" "x" IntT IntT (Plus (Int 1) (Var "x"))))])
    (is= (parse "fun mk (x: int): int -> int is fun add (y: int): int is x + y;;")
         [(Expr (Fun "mk" "x" IntT (ArrowT IntT IntT) (Fun "add" "y" IntT IntT (Plus (Var "x") (Var "y")))))])
    (is= (parse "fun eq3 (x: int): bool is 3 = x;; 1 + 2")
         (parse "fun eq3 (x: int): bool is 3 = x;; 1 + 2;;")
         [(Expr (Fun "eq3" "x" IntT BoolT (Equal (Int 3) (Var "x"))))
          (Expr (Plus (Int 1) (Int 2)))])
    (is (thrown? Exception (parse "fun eq3 (x: int): bool is 3 = x 1 + 2")))))
