(ns guess.core
  (:require [guess.synth :as synth]
            [clojure.string :as str]))

(defn parse-examples-file [fname]
  (let [[raw-valids raw-invalids] (str/split (slurp fname) #"\n\n")
        parser (fn [raw] (read-string (str "(" raw ")")))]
    [(map parser (str/split-lines raw-valids))
     (map parser (str/split-lines raw-invalids))]))

(defn solution? [valids invalids]
  (fn [f]
    (when (and (reduce (fn [x y] (and x y))
                       (map (fn [valid]
                              (eval `(~f ~@valid)))
                            valids))
               (reduce (fn [x y] (and (not x) (not y)))
                       (map (fn [invalid]
                              (eval `(~f ~@invalid)))
                            invalids)))
      f)))

(defn -main [& args]
  (let [candidates (synth/synthesize :arith-ops '(+ - * /)
                                     :comparison-ops '(< =)
                                     :bool-ops '(and or)
                                     :max-nesting 3
                                     :max-constant 5
                                     :n-variables 3)]
    (let [[valids invalids] (parse-examples-file (first args))]
      (println (some #(when ((solution? valids invalids) %) %)
                     ['(fn [x y z] false)
                      '(fn [x y z] true)])))))
