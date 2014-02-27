(ns guess.gen-arith)

(defn assoc-op [op]
  (get {'clojure.core/* *
        'clojure.core/+ +
        'clojure.core/- +
        'clojure.core// *}
       op))

(defn commutative? [op]
  (#{'+ '*} op))

(defn associate-chain
  "Associate a chain of operations, e.g a * 2 * 2 -> a * 4"
  [op x y]
  (cond
   (and (number? x)
        (coll? y)
        (= op (first y)))
   (if (number? (second y))
     `(~op ~(last y) ~((assoc-op op) (second y) x))
     (if (number? (last y))
       `(~op ~(second y) ~((assoc-op op) (last y) x))))

   (and (number? y)
        (coll? x)
        (= op (first x)))
   (if (number? (second x))
     `(~op ~(last x) ~((assoc-op op) (second x) y))
     (if (number? (last x))
       `(~op ~(second x) ~((assoc-op op) y (last x)))))))

(defn simplify-add [x y]
  (if-let [chained (associate-chain 'clojure.core/+ x y)]
    chained
    `(+ ~x ~y)))

(defn simplify-sub [x y]
  (if-let [chained (associate-chain 'clojure.core/- x y)]
    chained
    `(- ~x ~y)))

(defn simplify-mult [x y]
  (if-let [chained (associate-chain 'clojure.core/* x y)]
    chained
    `(* ~x ~y)))

(defn simplify-div [x y]
  (if-let [chained (associate-chain 'clojure.core// x y)]
    chained
    `(/ ~x ~y)))

(defn simplify
  "Return an equivalent expression with less nesting."
  [op x y]
  (case op
    + (simplify-add x y)
    - (simplify-sub x y)
    * (simplify-mult x y)
    / (simplify-div x y)
    `(~op ~x ~y)))

(defn build-exp
  "Short circuits operations. If not possible, delegate to simplify."
  [op x y]
  (case op
    + (cond
       (= x 0) y
       (= y 0) x
       ;; prefer [* x 2] over [+ x x], but try to simplify first
       (= x y) (simplify '* x 2)
       :else (simplify '+ x y))
    - (cond
       (= x y)
       0

       (and (number? x)
            (not (number? y)))
       (recur '+ y (* x -1))

       (and (number? y)
            (not (number? x)))
       (recur '+ x (* -1 y))

       :else (simplify '- x y))
    * (cond
       (or (= x 0) (= y 0)) 0
       (= x 1) y
       (= y 1) x
       :else (simplify '* x y))
    / (when-not (= y 0)
        (cond
         (= y 1) x
         (= x y) 1
         :else (simplify '/ x y)))
    (simplify op x y)))

(defn exps-with-ops
  [ops max-nesting numbers vars]
  (if (> max-nesting 1)
    (let [exps (exps-with-ops ops (dec max-nesting) numbers vars)]
      (mapcat (fn [op]
                (mapcat (fn [exp1]
                          (map (fn [exp2]
                                 (build-exp op exp1 exp2))
                               (if (commutative? op)
                                 (drop (.indexOf exps exp1) exps)
                                 exps)))
                        exps))
              ops))
    (mapcat (fn [op]
              (mapcat (fn [var1]
                        (concat (map (fn [var2]
                                       (build-exp op var1 var2))
                                     (if (commutative? op)
                                       (drop (.indexOf vars var1) vars)
                                       vars))
                                (map (fn [n]
                                       (build-exp op var1 n))
                                     numbers)))
                      vars))
            ops)))

(defn numbers
  [n]
  (range 1 (+ n 1)))

(defn all
  "Build all possible arithmetic expressions given certain restrictions.

These restrictions are:

1. Valid operators.
2. Maximum level of nesting.
3. Greatest natural number to use as a constant.
4. Maximum number of variables to use."
  [& {:keys [ops max-nesting max-constant variables]}]
  (let [nums (numbers max-constant)]
    (if (= max-nesting 0)
      (concat variables nums)
      (remove nil? (distinct (exps-with-ops ops max-nesting nums variables))))))
