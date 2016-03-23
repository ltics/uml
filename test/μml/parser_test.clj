(ns μml.parser-test
  (require [acolfut.sweet :refer :all]
           [μml.ast :refer :all]
           [μml.parser :refer :all]))

(deftest parser-test
  (testing "build ast"
    (is= (parse "(1 * 2)")
         (parse "1 * 2")
         [(Times (Int 1) (Int 2))])
    (is= (parse "1 + 2 * 3")
         [(Plus (Int 1) (Times (Int 2) (Int 3)))])
    (is= (parse "(1 + 2) * 3")
         [(Times (Plus (Int 1) (Int 2)) (Int 3))])))
