(ns guess.core
  (:require [guess.hypoth :as hypoth]
            [clojure.string :as str]))

(defn parse-examples-file
  "Read positive and negative examples from a file."
  [fname]
  (let [[raw-valids raw-invalids] (str/split (slurp fname) #"\n\n")
        parser (fn [raw] (read-string (str "(" raw ")")))]
    [(map parser (str/split-lines raw-valids))
     (map parser (str/split-lines raw-invalids))]))

(defn run-hypoth
  "Run a hypothesis against a set of test cases."
  [hypoth tests]
  (map (fn [test]
         (try
           (eval `(~hypoth ~@test))
           ;; In case an expression evaluates to (/ ? 0)
           (catch java.lang.ArithmeticException e
             false)))
       tests))

(defn valids-passed
  "Count the number of positive test cases passed."
  [hypoth valids]
  (count (keep #{true} (run-hypoth hypoth valids))))

(defn invalids-passed
  "Count the number of negative test cases passed."
  [hypoth invalids]
  (count (keep #{false} (run-hypoth hypoth invalids))))

(defn ratios
  "Return how many positive and negative tests a hypothesis passes."
  [hypoth valids invalids]
  (let [ratio-valids   (float (/ (valids-passed hypoth valids)
                                 (count valids)))
        ratio-invalids (float (/ (invalids-passed hypoth invalids)
                                 (count invalids)))]
    [ratio-valids ratio-invalids]))

(defn solution?
  "Test whether a hypothesis passes all tests."
  [ratio-valids ratio-invalids]
  (and (= 1.0 ratio-valids)
       (= 1.0 ratio-invalids)))

(defn anti-solution?
  "Test whether a hypothesis fails all tests."
  [ratio-valids ratio-invalids]
  (and (= 0.0 ratio-valids)
       (= 0.0 ratio-invalids)))

(defn guess
  "Find the boolean function that produces the correct output for the input.

  * get-hypoth is a closure of the kind returned by hypoth/hypotheses
  * valids is a seq of inputs for which a boolean function must output true
  * invalids is a seq for which said function must output false

  This function finds said function in the get-hypoth lazy seq, if there is one."
  [valids invalids get-hypoth]
  (let [[hypoth results-recorder] (get-hypoth)]
    (when hypoth
      (println (hypoth/body hypoth))
      (let [[ratio-valids ratio-invalids] (ratios hypoth valids invalids)]
        (if (solution? ratio-valids ratio-invalids)
          hypoth
          (if (anti-solution? ratio-valids ratio-invalids)
            (hypoth/negate hypoth)
            (recur valids
                   invalids
                   (results-recorder ratio-valids ratio-invalids))))))))

(defn -main [& args]
  (let [[valids invalids] (parse-examples-file (first args))
        result (guess valids invalids (hypoth/hypotheses :vars '(a b c)
                                                         :max-constant 5))]
    (println "\n\nSOLUTION")
    (println (hypoth/body result))))
