(ns guess.synth
  (:require [guess.gen-arith :as arith]
            [guess.gen-bool :as bool]))

(defn variables
  [n]
  (take n '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

(defn synth-one [exp vars]
  `(fn [~@vars]
     ~exp))

(defn synthesize
  "Take lisp expressions and output unevaluated functions whose bodies are said expressions."
  [&{:keys [arith-ops max-nesting max-constant n-variables comparison-ops bool-ops]}]
  (let [vars (variables n-variables)
        arith-exps (arith/all :ops arith-ops
                              :max-nesting max-nesting
                              :max-constant max-constant
                              :variables vars)
        bool-exps (bool/all :comparison-ops comparison-ops
                            :bool-ops bool-ops
                            :max-nesting max-nesting
                            :arith-exps arith-exps)]
    (map (fn [exp] (synth-one exp vars))
         bool-exps)))
