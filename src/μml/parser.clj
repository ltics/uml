(ns μml.parser
  (require [clj-antlr.core :as antlr]
           [instaparse.core :as insta]
           [adt.sweet :refer :all]
           [μml.ast :refer :all]))

(def parser (antlr/parser "resources/μml.g4"))

(defn parse
  [stream]
  (->> (antlr/parse parser stream)
       vec
       (insta/transform {:file  (fn [stat _] stat)
                         :top   (fn [& args]
                                  (as-> (first args) $
                                        (match $
                                          (Def _) $
                                          :else (Expr $))
                                        (cons $ (drop 1 args))
                                        (vec $)))
                         :def   (fn [& args]
                                  (let [[_ name _ expr] args]
                                    (Def name expr)))
                         :expr  (fn [& args]
                                  (condp = (count args)
                                    ;; function
                                    11 (let [[_ fun-name _ arg-name _ arg-type _ _ return-type _ body] args]
                                         (Fun fun-name arg-name arg-type return-type body))
                                    ;; condition
                                    6 (let [[_ predicate _ consequent _ alternative] args]
                                        (If predicate consequent alternative))
                                    ;; arith
                                    3 (let [expr1 (nth args 1)
                                            expr0 (nth args 0)
                                            expr2 (nth args 2)]
                                        (if (and (= expr0 "(")
                                                 (= expr2 ")"))
                                          expr1
                                          (cond
                                            (#{"*" "times"} expr1) (Times expr0 expr2)
                                            (#{"+" "plus"} expr1) (Plus expr0 expr2)
                                            (#{"-" "minus"} expr1) (Minus expr0 expr2)
                                            (#{"="} expr1) (Equal expr0 expr2)
                                            (#{"<"} expr1) (Less expr0 expr2)
                                            :else (throw (Exception. "invalid expression.")))))
                                    ;; atom
                                    1 (first args)
                                    (throw (Exception. "invalid expression."))))
                         :type  (fn [& args]
                                  (condp = (count args)
                                    ;; arrow type
                                    3 (let [[ltype _ rtype] args]
                                        (ArrowT ltype rtype))
                                    ;; simple type
                                    1 (first args)
                                    (throw (Exception. "invalid expression."))))
                         :atom  identity
                         :int   #(Int (Integer/parseInt %))
                         :bool  #(Bool (Boolean/parseBoolean %))
                         :var   #(Var %)
                         :intT  (fn [_] IntT)
                         :boolT (fn [_] BoolT)})
       flatten
       (filter #(not= ";;" %))
       vec))
