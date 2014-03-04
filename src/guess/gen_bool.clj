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

;; TODO Refactor - don't like a literal true in a conditional, let alone two of them.
(defn build-equal?
  "Determine whether to build the form (= x y) or not.

  Don't build if:

  * both x and y are arithmetic expressions, and
  * they apply the same operation, and
  * they have one operand in common that's not a number, and
    ** the other operands are equal to each other, or
    ** the other operands are both numbers.

  Examples of what not to build:

  (= (* a 3) (* 3 a))

  (= (* (- b 3) (+ a 5))
     (* (- b 3) (+ a 5)))"
  [x y]
  (if (same-op? x y)
    (if-let [[posx posy] (same-var? x y)]
      (and (not (= (nth x posx) (nth y posy)))
           (or (not (number? (nth x posx)))
               (not (number? (nth y posy)))))
      true)
    true))

;; TODO PLEASE refactor.
(defn build-lt?
  "Determine whether to build the form (< x y) or not.

  Don't build it if:

  * both x and y are arithmetic expressions, and
  * they apply the same operation, and
  * they have one operand in common that's not a number, and
  * the other operand is a number in both expressions.

  Examples of what not to build:

  (< (+ a 3) (+ a 1))
  (< (+ 3 a) (+ a 3))
  (< (* (+ a 3) 5) (* 12 (+ a 3)))"
  [x y]
  (if (same-op? x y)
    (if-let [[posx posy] (same-var? x y)]
      (not (and (number? (nth x posx))
                (number? (nth y posy))))
      true)
    true))

(defn build-comparison
  "Determine whether to build a comparison form or not.

  A comparison is not worth building if:

  * the operands are the same, or
  * they are both numbers

  If these two conditions are false, the decision is delegated to a function
  specific to the operator."
  [op x y]
  (when (and (not (= x y))
             (not (and (number? x)
                       (number? y))))
    (case op
      = (when (build-equal? x y)
          `(= ~x ~y))
      < (when (build-lt? x y)
          `(< ~x ~y))
      `(~op ~x ~y))))
