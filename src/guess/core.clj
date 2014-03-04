(ns guess.core
  (:require [guess.synth :as synth]
            [clojure.string :as str]))

(defn parse-examples-file [fname]
  (let [[raw-valids raw-invalids] (str/split (slurp fname) #"\n\n")
        parser (fn [raw] (read-string (str "(" raw ")")))]
    [(map parser (str/split-lines raw-valids))
     (map parser (str/split-lines raw-invalids))]))

(defn passes-valids? [candidate valids]
  (reduce (fn [x y]
            (and x y))
          (map (fn [valid]
                 (try
                   (eval `(~candidate ~@valid))
                   ;; In case an expression evaluates to (/ ? 0)
                   (catch java.lang.ArithmeticException e
                     false)))
               valids)))

(defn passes-invalids? [candidate invalids]
  (reduce (fn [x y] (and x (not y)))
          true
          (map (fn [invalid]
                 (try
                   (eval `(~candidate ~@invalid))
                   (catch java.lang.ArithmeticException e
                     true)))
               invalids)))

(defn guess [valids invalids candidates]
  (some (fn [candidate]
          (println (:body candidate))
          (when (and (passes-valids? (:unevaled-fn candidate) valids)
                     (passes-invalids? (:unevaled-fn candidate) invalids))
            candidate))
        candidates))

(defn -main [& args]
  (let [[valids invalids] (parse-examples-file (first args))
        result (guess valids invalids (synth/all :vars '(a b c)
                                                 :max-constant 5
                                                 :arith-ops '(+ - * /)))]
    (println "\n\nSOLUTION")
    (println (:body result))))
