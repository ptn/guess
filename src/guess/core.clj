(ns guess.core)

(defn commutative? [op]
  (#{+ * =} op))

(def arithops [+ - * /])
(def compops  [< =])
(def ops (concat arithops compops))

(defn can-associate-multiplication? [x y]
  (cond
   (and (number? x)
        (vector? y)
        (= * (first y)))
   (if (= (number? (second y)))
     [(last y) (* (second y) x)]
     (if (= (number? (last y)))
       [(second y) (* (last y) x)]))

   (and (number? y)
        (vector? x)
        (= * (first x)))
   (if (= (number? (second x)))
     [(last x) (* (second x) y)]
     (if (= (number? (last x)))
       [(second x) (* y (last x))]))))

(defn build [op x y]
  (condp = op
    + (if (= x 0)
        y
        (if (= y 0)
          x
          (if (= x y)
            [* x 2]
            [+ x y])))
    - (if (= x y)
        0
        [- x y])
    * (if (or (= x 0)
              (= y 0))
        0
        (if (= x 1)
          y
          (if (= y 1)
            x
            (if-let [result (can-associate-multiplication? x y)]
              [* (first result) (second result)]
              [* x y]))))
    / (when-not (= y 0)
        (if (= y 1)
          x
          (if (= x y)
            1
            [/ x y])))))

(defn arith-exps-with-ops
  [ops size numbers vars]
  (if (> size 1)
    (let [exps (arith-exps-with-ops ops (dec size) numbers vars)]
      (concat (mapcat (fn [op]
                        (mapcat (fn [exp1]
                                  (map (fn [exp2]
                                         (build op exp1 exp2))
                                       exps))
                                exps))
                      ops)
              exps))
    (mapcat (fn [op]
              (mapcat (fn [var1]
                        (concat (map (fn [var2]
                                       (build op var1 var2))
                                     vars)
                                (map (fn [n]
                                       (build op var1 n))
                                     numbers)))
                      vars))
            ops)))

(defn arith-numbers
  [n]
  (range 1 (+ n 1)))

(defn arith-vars
  [n]
  (take n [:a :b :c :d :e :f :g :h :i :j :k :l :m
           :n :o :p :q :r :s :t :u :v :w :x :y :z]))

(defn ariths
  [ops size max-n n-vars]
  (let [numbers (arith-numbers max-n)
        vars (arith-vars n-vars)]
    (distinct (concat (arith-exps-with-ops ops size numbers vars)
                      numbers
                      vars))))

(defn -main [& args]
  (println (take 10 (ariths arithops 3 20 3))))
