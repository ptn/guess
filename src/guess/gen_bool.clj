(ns guess.gen-bool)

(defn commutative? [op]
  (#{'=} op))

(defn- build [op x y]
  (when-not (= x y)
    `(~op ~x ~y)))

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
