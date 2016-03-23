(ns μml.util
  (require [clojure.tools.macro :refer [symbol-macrolet]]))

(defmacro letrec
  "Like let, but the bindings may be mutually recursive, provided that
   the heads of all values can be evaluated independently.
   This means that functions, lazy sequences, delays and the like can
   refer to other bindings regardless of the order in which they
   appear in the letrec form."
  [bindings & body]
  (let [bindings (destructure bindings)
        bcnt (quot (count bindings) 2)
        arrs (gensym "letrec_bindings_array__")
        arrv `(make-array Object ~bcnt)
        bprs (partition 2 bindings)
        bssl (map first bprs)
        bexs (map second bprs)
        arrm (zipmap bssl (range bcnt))]
    `(let [~arrs ~arrv]
       (symbol-macrolet [~@(mapcat (fn [s]
                                     [s `(aget ~arrs ~(arrm s))])
                                   bssl)]
                        ~@(map (fn [s e]
                                 `(aset ~arrs ~(arrm s) ~e))
                               bssl
                               bexs))
       (let [~@(mapcat (fn [s]
                         [s `(aget ~arrs ~(arrm s))])
                       bssl)]
         ~@body))))
