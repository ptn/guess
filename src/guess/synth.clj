(ns guess.synth
  (:require [guess.gen-arith :as arith]
            [guess.gen-bool :as bool]))

(defn numbers [max]
  (range 1 (+ 1 max)))

(defn rest-since [elt seq]
  (drop (+ 1 (.indexOf seq elt))
        seq))

(defn build-var-simple [op vars nums & {:keys [builder commutative?]}]
  (mapcat (fn [var]
            (let [vals (concat nums
                               (rest-since var vars))]
              (if (commutative? op)
                (map (fn [val]
                       (builder op var val))
                     vals)
                (mapcat (fn [val]
                          [(builder op var val)
                           (builder op val var)])
                        vals))))
          vars))

(defn build-var-arith [op vars exps & {:keys [builder commutative?]}]
  (mapcat (fn [var]
            (if (commutative? op)
              (map (fn [val]
                     (builder op var val))
                   exps)
              (mapcat (fn [val]
                        [(builder op var val)
                         (builder op val var)])
                      exps)))
          (drop 1 vars)))


(defn all-ariths [ops vars nums]
  (let [exps (mapcat (fn [op]
                       (build-var-simple op vars nums
                                         :builder arith/build-exp
                                         :commutative? arith/commutative?))
                     ops)
        nested (mapcat (fn [op]
                         (build-var-arith op vars exps
                                          :builder arith/build-exp
                                          :commutative? arith/commutative?))
                       ops)]
    (concat exps nested)))

(defn nest [op exps]
  (mapcat (fn [exp1]
            (map (fn [exp2]
                   `(~op ~exp1 ~exp2))
                 (rest-since exp1 exps)))
          exps))

(defn synth [body vars]
  `(fn [~@vars]
     ~body))

(defn body [fn]
  (first (nnext fn)))

(defn all
  [&{:keys [vars max-constant arith-ops]}]
  (let [nums (numbers max-constant)
        equal-var-simple (build-var-simple '= vars nums
                                           :builder bool/build-comparison
                                           :commutative? bool/commutative?)
        lt-var-simple (build-var-simple '< vars nums
                                        :builder bool/build-comparison
                                        :commutative? bool/commutative?)
        arith-exps (all-ariths arith-ops vars nums)
        equal-var-arith (build-var-arith '= vars arith-exps
                                         :builder bool/build-comparison
                                         :commutative? bool/commutative?)
        lt-var-arith (build-var-arith '< vars arith-exps
                                      :builder bool/build-comparison
                                      :commutative? bool/commutative?)
        fn-bodies (concat
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
    (distinct (remove nil? (map (fn [body] (synth body vars))
                                fn-bodies)))))
