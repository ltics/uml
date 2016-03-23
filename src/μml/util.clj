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

(comment

  (letrec [fibs (cons 0 (cons 1 (lazy-seq (map + fibs (rest fibs)))))]
    (take 10 fibs))
  ;; => (0 1 1 2 3 5 8 13 21 34)

  (letrec [x 1
           y 'x]
    y)
  ;; => x

  (letrec [ev? (fn [n] (if (zero? n) true (od? (dec n))))
           od? (fn [n] (if (zero? n) false (ev? (dec n))))]
    (ev? 11))
  ;; => false

  (letrec [xs (lazy-seq (filter even? ys)) ys (range 10)] xs)
  ;; => (0 2 4 6 8)

  (letrec [y (delay x) x 1]
    (let [y (delay :foo)]
      (force y)))
  ;; => :foo

  (letrec [[ev? od?]
           [(fn [n] (if (zero? n) true (od? (dec n))))
            (fn [n] (if (zero? n) false (ev? (dec n))))]]
    (ev? 10))
  ;; => true

  (letrec [[x y :as fibs]
           (cons 0 (cons 1 (lazy-seq (map + fibs (rest fibs)))))]
    [x y (take 10 fibs)])
  ;; => [0 1 (0 1 1 2 3 5 8 13 21 34)]

  (letrec [[f & fibs] (cons 0 (cons 1 (lazy-seq (map + (cons f fibs) fibs))))]
    (take 10 (cons f fibs)))
  ;; => (0 1 1 2 3 5 8 13 21 34)

  ;; NB. it must be possible to evaluate the heads of all the values
  ;; independently:

  (letrec [y (delay x) x 1] (force y))
  ;; => 1

  (letrec [y x x 1] y)
  ;; => nil

  (letrec [xs (filter even? (lazy-seq ys))
           ys (range)]
    (take 10 xs))
  ;; => (0 2 4 6 8 10 12 14 16 18)

  (letrec [xs (filter even? ys)
           ys (range)]
    (take 10 xs))
  ;; => ()

  )
