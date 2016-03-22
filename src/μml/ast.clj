(ns μml.ast
  (require [adt.sweet :refer :all]))

(defadt ::type
  IntT
  BoolT
  (ArrowT ltype rtype))

(defadt ::expr
  (Var name)
  (Int value)
  (Bool value)
  (Times expr1 expr2)
  (Plus expr1 expr2)
  (Minus expr1 expr2)
  (Equal expr1 expr2)
  (Less expr1 expr2)
  (If predicate consequent alternative)
  (Fun fun-name arg-name arg-type return-type body)
  (Apply expr1 expr2))

(defadt ::toplevel-cmd
  (Expr expr)
  (Def name expr))

(defn string-of-type
  [type]
  (match type
    IntT "int"
    BoolT "bool"
    (ArrowT ltype rtype) (match ltype
                           (ArrowT _) (format "(%s) -> %s"
                                                (string-of-type ltype)
                                                (string-of-type rtype))
                           :else (format "%s -> %s"
                                         (string-of-type ltype)
                                         (string-of-type rtype)))))

(defn string-of-expr
  [expr]
  (let [bin-op #(format "(%s %s %s)"
                        (string-of-expr %1)
                        %2
                        (string-of-expr %3))]
    (match expr
      (Var n) n
      {:value v} (str v)
      (Times e1 e2) (bin-op e1 "*" e2)
      (Plus e1 e2) (bin-op e1 "+" e2)
      (Minus e1 e2) (bin-op e1 "-" e2)
      (Equal e1 e2) (bin-op e1 "==" e2)
      (Less e1 e2) (bin-op e1 "<" e2)
      (If p c a) (format "(if %s %s %s)"
                         (string-of-expr p)
                         (string-of-expr c)
                         (string-of-expr a))
      (Fun fn an at rt b) (format "(ƒ %s (%s: %s): %s is %s)"
                                  fn
                                  an
                                  (string-of-type at)
                                  (string-of-type rt)
                                  (string-of-expr b))
      (Apply e1 e2) (format "(%s %s)"
                            (string-of-expr e1)
                            (string-of-expr e2)))))

(defn subst
  [env expr]
  (match expr
    (Var n) (or (env n) (Var n))
    (Int v) (Int v)
    (Bool v) (Bool v)
    (Times e1 e2) (Times (subst env e1) (subst env e2))
    (Plus e1 e2) (Plus (subst env e1) (subst env e2))
    (Minus e1 e2) (Minus (subst env e1) (subst env e2))
    (Equal e1 e2) (Equal (subst env e1) (subst env e2))
    (Less e1 e2) (Less (subst env e1) (subst env e2))
    (If p c a) (If (subst env p) (subst env c) (subst env a))
    (Fun fn an at rt b) (let [env' (dissoc env fn an)]
                          (Fun fn an at rt (subst env' b)))
    (Apply e1 e2) (Apply (subst env e1) (subst env e2))))
