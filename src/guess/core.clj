(ns guess.core
  (:require [guess.synth :as synth]
            [clojure.string :as str]))

(defn parse-examples-file [fname]
  (let [[raw-valids raw-invalids] (str/split (slurp fname) #"\n\n")
        parser (fn [raw] (read-string (str "(" raw ")")))]
    [(map parser (str/split-lines raw-valids))
     (map parser (str/split-lines raw-invalids))]))

(defn -main [& args]
  (let [candidates (synth/synthesize :arith-ops '(+ - * /)
                                     :comparison-ops '(< =)
                                     :bool-ops '(and or)
                                     :max-nesting 3
                                     :max-constant 5
                                     :n-variables 3)]
    (let [[valids invalids] (parse-examples-file (first args))]
      (println (type (first valids))))))
