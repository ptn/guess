(ns guess.core
  (:require [guess.gen-arith :as arith]
            [guess.gen-bool :as bool]))

(defn variables
  [n]
  (take n '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

(defn synth-one [exp vars]
  `(fn [~@vars]
     ~exp))

(defn synthesize
  "Take valid lisp expressions and output functions whose bodies are said expressions."
  [exps vars]
  (map (fn [exp] (synth-one exp vars))
       exps))

(defn -main [& args]
  (let [nesting 3
        vars (variables 3)
        boolops '(and or)
        compops '(< =)
        arithops '(+ - * /)
        arith-exps (arith/all :ops arithops
                              :max-nesting nesting
                              :max-n 5
                              :vars vars)
        bool-exps (bool/all :compops compops
                            :boolops boolops
                            :max-nesting nesting
                            :arith-exps arith-exps)]
    (let [fst (first (synthesize bool-exps vars))
          tuple (read-string (read-line))]
      (println (eval `(~fst ~@tuple))))))
