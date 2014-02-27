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
                              (try
                                (eval `(~f ~@valid))
                                (catch java.lang.ArithmeticException e
                                  false)))
                            valids))
               (reduce (fn [x y] (and x (not y)))
                       true
                       (map (fn [invalid]
                              (try
                                (eval `(~f ~@invalid))
                                (catch java.lang.ArithmeticException e
                                  true)))
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
                     candidates)))))
