(ns μml.compile
  (require [adt.sweet :refer :all]
           [μml.ast :refer :all]
           [μml.machine :refer :all])
  (:refer-clojure :exclude [pop compile]))

(defn compile
  "compiles program or a expression into a list of machine instructions."
  [expr]
  (let [arith (fn [e1 e2 instruction]
                `(~@(compile e1) ~@(compile e2) ~instruction))]
    (-> (match expr
          (Var x) `(~(VarI x))
          (Int v) `(~(IntI v))
          (Bool v) `(~(BoolI v))
          (Times e1 e2) (arith e1 e2 MultI)
          (Plus e1 e2) (arith e1 e2 AddI)
          (Minus e1 e2) (arith e1 e2 SubI)
          (Equal e1 e2) (arith e1 e2 EqualI)
          (Less e1 e2) (arith e1 e2 LessI)
          (If p c a) `(~@(compile p) ~(BranchI (compile c) (compile a)))
          (Fun fn an at rt e) (let [body (-> `(~@(compile e) ~PopEnvI) vec)]
                                `(~(ClosureI fn an body)))
          (Apply e1 e2) `(~@(compile e1) ~@(compile e2) ~CallI))
        vec)))
