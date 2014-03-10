(ns guess.synth
  (:require [guess.arith :as arith]
            [guess.bool  :as bool]))

(defn numbers [max]
  (range (+ 1 max)))

(defn rest-since [elt seq]
  (drop (+ 1 (.indexOf seq elt))
        seq))

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


(defn all-ariths
  "Generate all arith expressions according to the grammar."
  [vars nums]
  (let [ops '(+ - * /)
        exps (remove nil?
                     (mapcat (fn [op]
                               (build-var-simple op vars nums
                                                 :builder arith/build-exp
                                                 :commutative? arith/commutative?))
                             ops))
        nested (mapcat (fn [op]
                         (build-var-arith op vars exps
                                          :builder arith/build-exp
                                          :commutative? arith/commutative?))
                       ops)]
    (remove nil? (concat exps nested))))

(defn nest
  "Nest expressions given a boolean operator (like and)."
  [op exps]
  (mapcat (fn [exp1]
            (map (fn [exp2]
                   `(~op ~exp1 ~exp2))
                 (rest-since exp1 exps)))
          exps))

(defn synth-one
  "Construct an unevaluated function form given its params and body."
  [vars body]
  `(fn [~@vars]
     ~body))

(defn synth [vars bodies]
  (map (fn [body] (synth-one vars body))
       bodies))

(defn negate [exp]
  `(not ~exp))

(defn candidates
  "Generate all expressions in the following grammar:

  VAL              -> 'VAR | 'NUM

  EQUAL-VAR-SIMPLE -> (= 'VAR VAL)

  LT-VAR-SIMPLE    -> (< 'VAR VAL)
  LT-VAR-SIMPLE    -> (< VAL 'VAR)

  NESTED-EQ-V-S    -> (AND EQUAL-VAR-SIMPLE EQUAL-VAR-SIMPLE)
  NESTED-LT-V-S    -> (AND LT-VAR-SIMPLE LT-VAR-SIMPLE)

  OP               -> + | - | * | /
  ARITH-EXP        -> SIMPLE-ARITH | NESTED-ARITH
  SIMPLE-ARITH     -> (OP 'VAR VAL)
  NESTED-ARITH     -> (OP 'VAR SIMPLE-ARITH)

  EQUAL-VAR-ARITH  -> (= 'VAR ARITH-EXP)

  NESTED-EQ-V-A    -> (AND EQUAL-VAR-ARITH EQUAL-VAR-ARITH)

  LT-VAR-ARITH     -> (< 'VAR ARITH-EXP)

  NESTED-LT-V-A    -> (AND LT-VAR-ARITH LT-VAR-ARITH)
  "
  [&{:keys [vars max-constant]}]
  (let [nums (numbers max-constant)
        equal-var-simple (build-var-simple '= vars nums
                                           :builder bool/build-comparison
                                           :commutative? bool/commutative?)
        lt-var-simple (build-var-simple '< vars nums
                                        :builder bool/build-comparison
                                        :commutative? bool/commutative?)
        arith-exps (all-ariths vars nums)
        equal-var-arith (build-var-arith '= vars arith-exps
                                         :builder bool/build-comparison
                                         :commutative? bool/commutative?)
        lt-var-arith (build-var-arith '< vars arith-exps
                                      :builder bool/build-comparison
                                      :commutative? bool/commutative?)
        fn-bodies  (concat
                    ;; guesses (= a b); (= c 4)
                    equal-var-simple
                    ;; guesses (< b c); (< a 3)
                    lt-var-simple
                    ;; guesses (and (= a b) (= c 4))
                    (nest 'and equal-var-simple)
                    ;; guesses (and (< a b) (< b c))
                    (nest 'and lt-var-simple)
                    ;; guesses (= b (+ a 1)); (= c (* b 4))
                    equal-var-arith
                    ;; guesses (and (= b (+ a 1))
                    ;;              (= c (+ b 1)))
                    (nest 'and equal-var-arith)
                    ;; guesses (< c (+ b a))
                    lt-var-arith
                    ;; guesses (and (< b (+ a 1))
                    ;;              (< c (+ b 4)))
                    (nest 'and lt-var-arith))]
    (synth vars fn-bodies)))

(defn simplest
  [vars max-constant]
  (let [nums (numbers max-constant)]
    (synth vars
           (concat (build-var-simple '= vars nums
                                     :builder bool/build-comparison
                                     :commutative? bool/commutative?)
                   (build-var-simple '< vars nums
                                     :builder bool/build-comparison
                                     :commutative? bool/commutative?)))))

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
  )
