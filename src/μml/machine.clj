(ns μml.machine
  (require [adt.sweet :refer :all])
  (:refer-clojure :exclude [pop]))

(def throw-exp #(throw (Exception. %)))

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
  (let [throw-exp #(throw-exp (str "unknown " x))]
    (match envs
      EmptyEnvrionment (throw-exp)
      (NameMValues envs) (let [[env & _] envs]
                           (or (env x)
                               (throw-exp))))))

(defn pop
  [stack]
  (match stack
    EmptyStack (throw-exp "empty stack")
    (MValues mvalues) (let [[head & tail] mvalues]
                        [head tail])))

(defn pop-bool
  [stack]
  (let [throw-exp #(throw-exp "bool expected")]
    (match stack
      EmptyStack (throw-exp)
      (MValues mvalues) (let [[head & tail] mvalues]
                          (match head
                            (BoolM v) [head tail]
                            :else (throw-exp))))))

(defn pop-app
  [stack]
  (let [throw-exp #(throw-exp "value and closure expected")]
    (match stack
      EmptyStack (throw-exp)
      (MValues mvalues) (let [[v closure & t] mvalues]
                          (match closure
                            (ClosureT fn an e) [fn an e v t]
                            :else (throw-exp))))))

;; arith operations take their arguments from a stack and put the result back onto the stack.

(defn arith-op
  [mvalue op op-name]
  (fn [stack]
    (let [throw-exp #(throw-exp (str "int and int expected in " op-name))]
      (match stack
        EmptyStack (throw-exp)
        (MValues mvalues) (let [[x y & t] mvalues]
                            (match x
                              (mvalue v1) (match y
                                          (mvalue v2) (cons (mvalue (op v1 v2)) t)
                                          :else (throw-exp))
                              :else (throw-exp)))))))

(def mult (arith-op IntM * "mult"))
(def add (arith-op IntM + "add"))
(def sub (arith-op IntM - "sub"))
(def equal (arith-op BoolM = "equal"))
(def less (arith-op BoolM < "less"))

(defn exec
  "executes instruction in the given state [frames, stack, envs],
   where frames is a list of frames,
   stack is just a stack, a list of machine values,
   envs is a list of environments,
   The return value is a new state."
  [instruction frames stack envs]
  (match instruction
    ;; arithmetic
    MultI [frames (mult stack) envs]))
