(ns μml.machine
  (require [adt.sweet :refer :all]))

;; machine values
(defadt ::mvalue
  (IntM value)
  (BoolM value)
  (ClosureM name frame env))

;; machine instructions
(defadt ::instruction
  MultI
  AddI
  SubI
  EqualI
  LessI
  (VarI name)
  (IntI value)
  (BoolI value)
  (ClosureI fun-name arg-name frame)
  (BranchI consequent alternative)
  CallI
  PopEnvI)

;; a frame is just a list (stack) of instructions
(defadt ::frame
  EmptyFrame
  (Instructions instructions))

;; a environment is just a list of names to mvalues single pairs
(defadt ::environment
  EmptyEnvrionment
  (NameMValues name-mvalues))

;; a stack is just a list of mvalues
(defadt ::stack
  EmptyStack
  (MValues mvalues))

(defn string-of-mvalue
  [mvalue]
  (match mvalue
    (IntM v) (str v)
    (BoolM v) (str v)
    (ClosureM _) "<ƒ>"))

(defn lookup
  [x envs]
  (let [throw-exp #(throw (Exception. (str "unknown " x)))]
    (match envs
      EmptyEnvrionment (throw-exp)
      (NameMValues envs) (let [[env & _] envs]
                           (or (env x)
                               (throw-exp))))))

(defn pop
  [stack]
  (match stack
    EmptyStack (throw (Exception. "empty stack"))
    (MValues mvalues) (let [[head & tail] mvalues]
                        [head (vec tail)])))

(defn pop-bool
  [stack]
  (let [throw-exp #(throw (Exception. "bool expected"))]
    (match stack
      EmptyStack (throw-exp)
      (MValues mvalues) (let [[head & tail] mvalues]
                          (match head
                            (BoolM v) [head (vec tail)]
                            :else (throw-exp))))))

(defn pop-app
  [stack]
  (let [throw-exp #(throw (Exception. "value and closure expected"))]
    (match stack
      EmptyStack (throw-exp)
      (MValues mvalues) (let [[v closure & t]]
                          (match closure
                            (ClosureT fn an e) [fn an e v t]
                            :else (throw-exp))))))
