(ns μml.machine
  (require [adt.sweet :refer :all]))

;; machine values
(defadt ::mvalue
  (IntM value)
  (BoolM value)
  (ClosureM name frame env))

;; machine instructions
(defadt ::instr
  MultI
  AddI
  SubI
  EqualI
  LessI
  (VarI name)
  (IntI value)
  (BoolI value)
  (ClosureI name name frame)
  (CallI frame frame)
  PopEnvI)
