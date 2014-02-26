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
e.g. '(+ a 8) and '(* 9 a). Returns the positions of the other operands."
  [x y]
  (cond
   (and (symbol? (second x))
        (symbol? (second y))
        (= (second x) (second y)))
   [2 2]

   (and (symbol? (second x))
        (symbol? (last y))
        (= (second x) (last y)))
   [2 1]

   (and (symbol? (last x))
        (symbol? (second y))
        (= (last x) (second y)))
   [1 2]

   (and (symbol? (last x))
        (symbol? (last y))
        (= (last x) (last y)))
   [1 1]))

(defn different-other-operand?
  "Verifies if x and y are expressions that apply and arithmetic operation
to the same operands, e.g. '(+ a 8) '(- a 8) "
  [x y pos-other-x pos-other-y]
  (not (= (nth x pos-other-x) (nth y pos-other-y))))

;; TODO Refactor.
(defn build-equal? [x y]
  (if (same-op? x y)
    (if-let [[posx posy] (same-var? x y)]
      (different-other-operand? x y posx posy)
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
