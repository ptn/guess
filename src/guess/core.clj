(ns guess.core
  (:require [guess.synth :as synth]
            [clojure.string :as str]))

(defn parse-examples-file
  "Read positive and negative examples from a file."
  [fname]
  (let [[raw-valids raw-invalids] (str/split (slurp fname) #"\n\n")
        parser (fn [raw] (read-string (str "(" raw ")")))]
    [(map parser (str/split-lines raw-valids))
     (map parser (str/split-lines raw-invalids))]))

(defn passes-valids?
  "Verify that candidate outputs true for every test case in valids."
  [candidate valids]
  (reduce (fn [x y]
            (and x y))
          (map (fn [valid]
                 (try
                   (eval `(~candidate ~@valid))
                   ;; In case an expression evaluates to (/ ? 0)
                   (catch java.lang.ArithmeticException e
                     false)))
               valids)))

(defn passes-invalids?
  "Verify that candidate outputs false for every test case in invalids."
  [candidate invalids]
  (reduce (fn [x y] (and x (not y)))
          true
          (map (fn [invalid]
                 (try
                   (eval `(~candidate ~@invalid))
                   (catch java.lang.ArithmeticException e
                     true)))
               invalids)))

(defn guess
  "Find the boolean function that produces the correct output for the input.

  * candidates is a lazy seq of unevaluated function forms
  * valids is seq of inputs for which a boolean function must output true
  * invalids is a seq for which said function must output false

  This function finds said function in the candidates seq, if there is one."
  [valids invalids candidates]
  (some (fn [candidate]
          (println (synth/body candidate))
          (when (and (passes-valids? candidate valids)
                     (passes-invalids? candidate invalids))
            candidate))
        candidates))

(defn -main [& args]
  (let [[valids invalids] (parse-examples-file (first args))
        result (guess valids invalids (synth/all :vars '(a b c)
                                                 :max-constant 5
                                                 :arith-ops '(+ - * /)))]
    (println "\n\nSOLUTION")
    (println (synth/body result))))
