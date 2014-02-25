(ns guess.gen-arith)

(defn commutative? [op]
  (#{'+ '*} op))

(defn- can-associate-multiplication? [x y]
  (cond
   (and (number? x)
        (coll? y)
        (= 'clojure.core/* (first y)))
   (if (number? (second y))
     [(last y) (* (second y) x)]
     (if (number? (last y))
       [(second y) (* (last y) x)]))

   (and (number? y)
        (coll? x)
        (= 'clojure.core/* (first x)))
   (if (number? (second x))
     [(last x) (* (second x) y)]
     (if (number? (last x))
       [(second x) (* y (last x))]))))

(defn- build-exp [op x y]
  (case op
    + (cond
       (= x 0) y
       (= y 0) x
       ;; prefer [* x 2] over [+ x x], but try to simplify first
       (= x y) (if-let [result (can-associate-multiplication? x 2)]
                 `(* ~(first result) ~(second result))
                 `(* ~x 2))
       :else `(+ ~x ~y))
    - (cond
       (= x y) 0
       :else `(- ~x ~y))
    * (cond
       (or (= x 0) (= y 0)) 0
       (= x 1) y
       (= y 1) x
       :else (if-let [result (can-associate-multiplication? x y)]
               `(* ~(first result) ~(second result))
               `(* ~x ~y)))
    / (when-not (= y 0)
        (cond
         (= y 1) x
         (= x y) 1
         :else `(/ ~x ~y)))
    `(~op ~x ~y)))

(defn- exps-with-ops
  [ops max-nesting numbers vars]
  (if (> max-nesting 1)
    (let [exps (exps-with-ops ops (dec max-nesting) numbers vars)]
      (concat exps
              (mapcat (fn [op]
                        (mapcat (fn [exp1]
                                  (map (fn [exp2]
                                         (build-exp op exp1 exp2))
                                       (if (commutative? op)
                                         (drop (.indexOf exps exp1) exps)
                                         exps)))
                                exps))
                      ops)))
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
    (remove nil? (distinct (concat vs
                                   nums
                                   (exps-with-ops ops max-nesting nums vs))))))
