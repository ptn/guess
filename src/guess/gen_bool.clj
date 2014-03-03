(ns guess.gen-bool)

(defn commutative? [op]
  (#{'=} op))

(defn same-op?
  "Tests whether x and y apply the same arithmetic operation to its operands."
  [x y]
  (and (coll? x)
       (coll? y)
       (= (first x) (first y))))

;; TODO Unfuck this function.
;; 1. Perform less comparisons.
;; 2. This is tightly coupled to different-other-operand?, because its return
;;    value is specifically tailored to be used by it.
(defn same-var?
  "Tests whether x and y apply an arithmetic operation to the same variable,
e.g. '(+ a 8) and '(* 9 a) or even '(+ (* x 3) 8) and '(+ (* x 3) 12)."
  [x y]
  (cond
   (and (not (number? (second x)))
        (not (number? (second y)))
        (= (second x) (second y)))
   [2 2]

   (and (not (number? (second x)))
        (not (number? (last y)))
        (= (second x) (last y)))
   [2 1]

   (and (not (number? (last x)))
        (not (number? (second y)))
        (= (last x) (second y)))
   [1 2]

   (and (not (number? (last x)))
        (not (number? (last y)))
        (= (last x) (last y)))
   [1 1]))

;; TODO Refactor.
(defn build-equal? [x y]
  (if (same-op? x y)
    (if-let [[posx posy] (same-var? x y)]
      (and (not (= (nth x posx) (nth y posy)))
           (or (not (number? (nth x posx)))
               (not (number? (nth y posy)))))
      true)
    true))

;; TODO PLEASE refactor.
(defn build-lt? [x y]
  (if (same-op? x y)
    (if-let [[posx posy] (same-var? x y)]
      (not (and (number? (nth x posx))
                (number? (nth y posy))))
      true)
    true))

(defn build-comparison [op x y]
  (when (and (not (= x y))
             (not (and (number? x)
                       (number? y))))
    (case op
      = (when (build-equal? x y)
          `(= ~x ~y))
      < (when (build-lt? x y)
          `(< ~x ~y))
      `(~op ~x ~y))))
