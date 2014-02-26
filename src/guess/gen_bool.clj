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

(defn build [op x y]
  (when-not (= x y)
    (case op
      = (when (build-equal? x y)
          `(= ~x ~y))
      `(~op ~x ~y))))

(defn all
  [& {:keys [ops max-nesting arith-exps]}]
  (let [raw-exps (mapcat(fn [op]
                          (mapcat (fn [exp1]
                                    (map (fn [exp2]
                                           (build op exp1 exp2))
                                         (if (commutative? op)
                                           (drop (+ 1 (.indexOf arith-exps exp1))
                                                 arith-exps)
                                           arith-exps)))
                                  arith-exps))
                        ops)]
    (remove nil? (distinct raw-exps))))
