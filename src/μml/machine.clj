(ns μml.machine
  (require [adt.sweet :refer :all]
           [letrec.sweet :refer :all])
  (:refer-clojure :exclude [pop loop]))

(def throw-exp #(throw (Exception. %)))

;; machine values
(defadt ::mvalue
  (IntM value)
  (BoolM value)
  (ClosureM arg-name frame env))

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
                        [head (vec tail)])))

(defn pop-bool
  [stack]
  (let [throw-exp #(throw-exp "bool expected")]
    (match stack
      EmptyStack (throw-exp)
      (MValues mvalues) (let [[head & tail] mvalues]
                          (match head
                            (BoolM v) [v (vec tail)]
                            :else (throw-exp))))))

(defn pop-app
  [stack]
  (let [throw-exp #(throw-exp "value and closure expected")]
    (match stack
      EmptyStack (throw-exp)
      (MValues mvalues) (let [[v closure & t] mvalues]
                          (match closure
                            (ClosureM an frame env) [an frame env v (vec t)]
                            :else (throw-exp))))))

;; arith operations take their arguments from a stack and put the result back onto the stack.

(defn put-top
  "put back on top of a stack"
  [h t]
  (-> h
      (cons t)
      vec))

(defn arith-op
  [mvalue op op-name]
  (fn [stack]
    (let [throw-exp #(throw-exp (str "int and int expected in " op-name))]
      (match stack
        EmptyStack (throw-exp)
        (MValues mvalues) (let [[x y & t] mvalues]
                            (match x
                              (mvalue v1) (match y
                                            (mvalue v2) (put-top (mvalue (op v1 v2)) t)
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
    ;; arithmetic operators
    MultI [frames (mult stack) envs]
    AddI [frames (add stack) envs]
    SubI [frames (sub stack) envs]
    EqualI [frames (equal stack) envs]
    LessI [frames (less stack) envs]
    ;; push values onto stack
    (VarI x) [frames (put-top (lookup x envs) stack) envs]
    (IntI v) [frames (put-top (IntM v) stack) envs]
    (BoolI v) [frames (put-top (BoolM v) stack) envs]
    (ClosureI fn an frame) (match envs
                             EmptyEnvrionment (throw-exp "no environment for a closure")
                             (NameMValues pairs) (let [[env & _] envs]
                                                   (letrec [c (ClosureM an
                                                                        frame
                                                                        (put-top {fn c} env))]
                                                     [frames (put-top c stack) envs])))
    ;; control instructions
    (BranchI c a) (let [[b stack'] (pop-bool stack)]
                    [(put-top (if b c a) frames) stack' envs])
    CallI (let [[an frame env v stack'] (pop-app stack)]
            [(put-top frame frames) stack' (put-top (put-top {an v} env) envs)])
    PopEnvI (match envs
              EmptyEnvrionment (throw-exp "no environment to pop")
              (NameMValues pairs) (let [[_ & envs'] envs]
                                    [frames stack envs']))))

(defn run
  "execute the frame (a list of instructions) in the specific environment."
  [frame env]
  (letrec [loop (fn [frames stack envs]
                  (if (and (empty? frames))
                    (if (= (count stack) 1)
                      (first stack)
                      (throw-exp "illegal end of program"))
                    (let [[frame & frames'] frames]
                      (if-not (empty? frame)
                        (let [[instruction & frame'] frame]
                          (loop (exec instruction (put-top (vec frame') frames') stack envs)))
                        (loop frames' stack envs)))))]
    (loop [frame] [] [env])))