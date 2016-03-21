(ns μml.type-check
  (require [adt.sweet :refer :all]
           [μml.ast :refer :all]))

(declare type-of)

(defn check
  [ctx type expr]
  (let [type' (type-of ctx expr)]
    (if (not= type type')
      (throw (Exception. (format "%s has type %s but is used as if it has type %s"
                                 (string-of-expr expr)
                                 (string-of-type type')
                                 (string-of-type type))))
      true)))

(defn type-of
  [ctx expr]
  (match expr
    (Var n) (if-let [type (ctx n)]
              type
              (throw (Exception. (format "unknown variable %s" n))))
    (Int _) IntT
    (Bool _) BoolT
    (Times e1 e2) (do (check ctx IntT e1)
                      (check ctx IntT e2)
                      IntT)
    (Plus e1 e2) (do (check ctx IntT e1)
                     (check ctx IntT e2)
                     IntT)
    (Minus e1 e2) (do (check ctx IntT e1)
                      (check ctx IntT e2)
                      IntT)
    (Equal e1 e2) (do (check ctx IntT e1)
                      (check ctx IntT e2)
                      BoolT)
    (Less e1 e2) (do (check ctx IntT e1)
                     (check ctx IntT e2)
                     BoolT)
    (If p c a) (do (check ctx BoolT p)
                   (let [type (type-of ctx c)]
                     (check ctx type a)
                     type))
    (Fun fn an at rt e) (do (let [ctx' (merge ctx
                                              {fn (ArrowT at rt)
                                               an at})]
                              (check ctx' rt e))
                            (ArrowT at rt))
    (Apply e1 e2) (let [t1 (type-of ctx e1)]
                    (match t1
                      (ArrowT at rt) (do (check ctx at e2)
                                         rt)
                      :else (throw (Exception. (format "%s is used as a function but its type is %s"
                                                       (string-of-expr e1)
                                                       (string-of-type t1))))))))
