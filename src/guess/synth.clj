(ns guess.synth
  (:require [guess.gen-arith :as arith]
            [guess.gen-bool :as bool]))

(defn variables
  [n]
  (take n '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

(defn synth-one [body vars]
  `(fn [~@vars]
     ~body))

(defn synthesize
  "Take lisp expressions and output unevaluated functions whose bodies are said expressions."
  [&{:keys [arith-ops max-nesting max-constant n-variables comparison-ops bool-ops]}]
  (let [vars (variables n-variables)
        fn-bodies (mapcat (fn [arith-nesting]
                            (let [arith-exps (arith/all :ops arith-ops
                                                        :max-nesting arith-nesting
                                                        :max-constant max-constant
                                                        :variables vars)]
                              (mapcat (fn [bool-nesting]
                                        (bool/all :bool-ops bool-ops
                                                  :comparison-ops comparison-ops
                                                  :max-nesting bool-nesting
                                                  :arith-exps arith-exps))
                                      (range max-nesting))))
                          (range (+ 1 max-nesting)))]
    (map (fn [body] (synth-one body vars))
         fn-bodies)))
