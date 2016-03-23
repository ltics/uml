(ns μml.parser
  (require [clj-antlr.core :as antlr]
           [instaparse.core :as insta]
           [μml.ast :refer :all]))

(def parser (antlr/parser "resources/μml.g4"))

(defn parse
  [stream]
  (->> (antlr/parse parser stream)
       vec
       (insta/transform {:file   (fn [stat _] stat)
                         :stat   (fn [& args] (vec args))
                         :expr   (fn [& args] (condp = (count args)
                                                3 (let [expr1 (nth args 1)
                                                        expr0 (nth args 0)
                                                        expr2 (nth args 2)]
                                                    (if (and (= expr0 "(")
                                                             (= expr2 ")"))
                                                      expr1
                                                      (cond
                                                        (#{"*" "times"} expr1) (Times expr0 expr2)
                                                        (#{"+" "plus"} expr1) (Plus expr0 expr2)
                                                        (#{"-" "minus" expr1}) (Minus expr0 expr2)
                                                        (= "=" expr1) (Equal expr0 expr2)
                                                        (= "<" expr1) (Less expr0 expr2)
                                                        :else (throw (Exception. "invalid expression.")))))
                                                1 (Int (first args))
                                                (throw (Exception. "invalid expression."))))
                         :atom   identity
                         :number #(Integer/parseInt %)})))
