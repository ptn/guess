(ns guess.core
  (:require [guess.gen-arith :as arith]))

(def arithops [+ - * /])

(defn -main [& args]
  (println (take 10 (arith/all :ops arithops
                               :max-nesting 3
                               :max-n 5
                               :n-vars 3))))
