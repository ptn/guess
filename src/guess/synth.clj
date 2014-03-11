(ns guess.synth
  (:require [guess.arith :as arith]
            [guess.bool  :as bool]))

(defn numbers [max]
  (range (+ 1 max)))

(defn rest-since [elt seq]
  (drop (+ 1 (.indexOf seq elt))
        seq))


(defn synth-one
  "Construct an unevaluated function form given its params and body."
  [vars body]
  `(fn [~@vars]
     ~body))

(defn synth [vars bodies]
  (map (fn [body] (synth-one vars body))
       bodies))

(defn build-var-simple
  "Build all forms (op var val) for every var, where val is either a var or a number.

  :builder is the actual function that builds the forms

  :commutative? is a function that determines whether the operator is
  commutative or not."
  [op vars nums & {:keys [builder commutative?]}]
  (let [map-over-vars (fn [other & {:keys [use-rest-since?]
                                    :or {use-rest-since? false}}]
                        (let [coll (if use-rest-since?
                                     (rest-since other vars)
                                     vars)]
                          (if (commutative? op)
                            (map (fn [var]
                                   (builder op var other))
                                 coll)
                            (mapcat (fn [var]
                                      [(builder op var other)
                                       (builder op other var)])
                                    coll))))]
    (distinct (remove nil? (concat (mapcat (fn [n] (map-over-vars n))
                                            nums)
                                    (mapcat (fn [var] (map-over-vars var :use-rest-since? true))
                                            vars))))))

(defn build-var-arith
  "Build all forms (op var exp) for every var, where exp is an arithmetic operation.

  :builder is the actual function that builds the forms

  :commutative? is a function that determines whether the operator is
  commutative or not."
  [op vars exps & {:keys [builder commutative?]}]
  (distinct (remove nil? (mapcat (fn [var]
                                   (if (commutative? op)
                                     (map (fn [val]
                                            (builder op var val))
                                          exps)
                                     (mapcat (fn [val]
                                               [(builder op var val)
                                                (builder op val var)])
                                             exps)))
                                 (drop 1 vars)))))


(defn simplest-exprs [vars max-constant]
  (let [nums (numbers max-constant)]
    (concat (build-var-simple '= vars nums
                              :builder bool/build-exp
                              :commutative? bool/commutative?)
            (build-var-simple '< vars nums
                              :builder bool/build-exp
                              :commutative? bool/commutative?))))

(defn nested-exps
  "Nest an expression with the simplest hypotheses using a boolean operator."
  [op exp vars max-constant]
  (map (fn [s] `(~op ~exp ~s))
       (simplest-exprs vars max-constant)))

(defn nest [op exp vars max-constant]
  (synth vars (nested-exps op exp vars max-constant)))

(defn negated-exp [exp]
  `(not ~exp))

(defn negate [vars exp]
  (synth-one vars (negated-exp exp)))

(defn simplest [vars max-constant]
  (synth vars (simplest-exprs vars max-constant)))

(defn more-wrong-than-right? [& ratios]
  (< (apply + ratios) 1.0))

(defn children-exps [exp])

(defn expand
  [result &{:keys [vars max-constant]}]
  (synth vars (cond
               (= 1.0 (:ratio-valids result))
               (nested-exps 'and (:body result) vars max-constant)

               (= 1.0 (:ratio-invalids result))
               (nested-exps 'or (:body result) vars max-constant)

               (more-wrong-than-right? (:ratio-valids result) (:ratio-invalids result))
               [(negated-exp (:body result))]

               :else (children-exps (:body result) vars max-constant))))

(defn from
  "Synthesize new boolean expressions given the results for a previous one.

  Arguments:

  * result - a map containing:
    * :hypoth - the expression to expand
    * :ratio-valids - ratio of positive test cases it passed
    * :ratio-invalids - ratio of negative test cases it passed
  * seen - expressions already generated, queried to avoid repeated values

  Keyword arguments:

  * :vars - vars used when generating expressions
  * :max-constant - max number allowed to use as a constant in an expression"
  [result seen &{:keys [vars max-constant]}]
  (remove seen (expand result :vars vars :max-constant max-constant)))
