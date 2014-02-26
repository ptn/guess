(ns guess.core
  (:require [guess.gen-arith :as arith]
            [guess.gen-bool :as bool]))

(def arithops '(+ - * /))
(def compops '(< =))
(def boolops '(and or))

(defn -main [& args]
  (let [nesting 3]
    (println (take 10 (bool/all :compops compops
                                :boolops boolops
                                :max-nesting nesting
                                :arith-exps (arith/all :ops arithops
                                                       :max-nesting nesting
                                                       :max-n 5
                                                       :n-vars 3))))))
