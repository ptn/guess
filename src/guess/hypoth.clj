(ns guess.hypoth
  (:require [guess.synth :as synth]))

(defn body
  "Return the body of a hypothesis."
  [hypoth]
  (first (nnext hypoth)))

(defn next-batch
  "Generate next batch of hypotheses to test."
  [seen latest-results vars max-constant]
  (if (empty? seen)
    (synth/simplest vars max-constant)))

(defn hypotheses
  "Returns a closure that returns:

  * the next hypothesis to test
  * the closure that needs to be invoked to record how well the previous
  hypothesis performed. This recorder closure in turn returns a new
  closure of the type that this function returns."
  [&{:keys [vars max-constant seen results queue]
     :or {seen [] results [] queue []}}]
  (let [[queue results] (if (empty? queue)
                          [(next-batch seen (first results) vars max-constant)
                           (rest results)]
                          [queue results])
        hypoth (first queue)]
    (fn []
      [hypoth (fn [ratio-valids ratio-invalids]
                (hypotheses :vars vars
                            :max-constant max-constant
                            :seen (conj seen hypoth)
                            :results (conj results {:hypoth hypoth
                                                    :ratio-valids ratio-valids
                                                    :ratio-invalids ratio-invalids})
                            :queue (rest queue)))])))
