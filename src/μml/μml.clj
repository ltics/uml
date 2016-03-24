(ns μml.μml
  (require [adt.sweet :refer :all]
           [μml.ast :refer :all]
           [μml.parser :refer :all]
           [μml.type-check :refer :all]
           [μml.machine :refer :all]
           [μml.compile :refer :all])
  (:refer-clojure :exclude [pop compile]))

;; a context is a mapping from globally defined names to types
(defadt ::context
  EmptyContext
  (CtxNameTypes name-types))

(defn exec-cmd
  "executes a top level command
   returns new ctx-env pair and the evaluation result"
  [[ctx env] cmd]
  (match cmd
    (Expr expr) (let [type   (type-of ctx expr)
                      frame  (compile expr)
                      mvalue (run frame env)]
                  [[ctx, env]
                   (format "- : %s = %s"
                           (string-of-type type)
                           (string-of-mvalue mvalue))])
    (Def name expr) (let [type (type-of ctx expr)
                          frame (compile expr)
                          mvalue (run frame env)]
                      [[(put-top {name type} ctx) (put-top {name mvalue} env)]
                       (format "%s : %s = %s"
                               name
                               (string-of-type type)
                               (string-of-mvalue mvalue))])))

(defn -main
  [& args])
