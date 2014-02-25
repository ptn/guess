(ns guess.gen-arith)

(defn assoc-with [op]
  (get {'clojure.core/* *
        'clojure.core/+ +
        'clojure.core/- +
        'clojure.core// *}
       op))

(defn commutative? [op]
  (#{'+ '*} op))

(defn- assoc-chain [op x y]
  (cond
   (and (number? x)
        (coll? y)
        (= op (first y)))
   (if (number? (second y))
     `(~op ~(last y) ~((assoc-with op) (second y) x))
     (if (number? (last y))
       `(~op ~(second y) ~((assoc-with op) (last y) x))))

   (and (number? y)
        (coll? x)
        (= op (first x)))
   (if (number? (second x))
     `(~op ~(last x) ~((assoc-with op) (second x) y))
     (if (number? (last x))
       `(~op ~(second x) ~((assoc-with op) y (last x)))))))

(defn- assoc-add [x y]
  (assoc-chain 'clojure.core/+ x y))

(defn- assoc-sub [x y]
  (assoc-chain 'clojure.core/- x y))

(defn- assoc-mult [x y]
  (assoc-chain 'clojure.core/* x y))

(defn- assoc-div [x y]
  (assoc-chain 'clojure.core// x y))

(defn associate [op x y]
  (case op
    + (if-let [result (assoc-add x y)]
        result
        `(+ ~x ~y))
    - (if-let [result (assoc-sub x y)]
        result
        `(- ~x ~y))
    * (if-let [result (assoc-mult x y)]
        result
        `(* ~x ~y))
    / (if-let [result (assoc-div x y)]
        result
        `(/ ~x ~y))
    `(~op ~x ~y)))

(defn- build-exp [op x y]
  (case op
    + (cond
       (= x 0) y
       (= y 0) x
       ;; prefer [* x 2] over [+ x x], but try to simplify first
       (= x y) (associate '* x 2)
       :else (associate '+ x y))
    - (cond
       (= x y) 0
       :else (associate '- ~x ~y))
    * (cond
       (or (= x 0) (= y 0)) 0
       (= x 1) y
       (= y 1) x
       :else (associate '* x y))
    / (when-not (= y 0)
        (cond
         (= y 1) x
         (= x y) 1
         :else (associate '/ ~x ~y)))
    (associate op x y)))

(defn- exps-with-ops
  [ops max-nesting numbers vars]
  (if (> max-nesting 1)
    (let [exps (exps-with-ops ops (dec max-nesting) numbers vars)]
      (concat (mapcat (fn [op]
                        (mapcat (fn [exp1]
                                  (map (fn [exp2]
                                         (build-exp op exp1 exp2))
                                       (if (commutative? op)
                                         (drop (.indexOf exps exp1) exps)
                                         exps)))
                                exps))
                      ops)
              exps))
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

(defn- numbers
  [n]
  (range 1 (+ n 1)))

(defn- vars
  [n]
  (take n '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

(defn all
  "Build all possible arithmetic expressions given certain restrictions.

These restrictions are:

1. Valid operators.
2. Maximum level of nesting.
3. Greatest natural number to use as a constant.
4. Maximum number of variables to use."
  [& {:keys [ops max-nesting max-n n-vars]}]
  (let [nums (numbers max-n)
        vs (vars n-vars)]
    (remove nil? (distinct (concat (exps-with-ops ops max-nesting nums vs)
                                   nums
                                   vs)))))
