(ns guess.hypoth
  (:require [guess.synth :as synth]))

(defn body
  "Return the body of a hypothesis."
  [hypoth]
  (first (nnext hypoth)))

(defn args
  "Return the list of arguments a hypothesis takes."
  [hypoth]
  (second hypoth))

(defn negate
  "Produce (not HYPOTH)"
  [hypoth]
  (synth/negate (args hypoth) (body hypoth)))

(defn next-batch
  "Generate next batch of hypotheses to test."
  [result seen vars max-constant]
  (if (empty? seen)
    (synth/simplest vars max-constant)
    (synth/from result
                seen
                :vars vars
                :max-constant max-constant)))

(defn hypotheses
  "Returns a closure that returns:

  * the next hypothesis to test
  * the closure that needs to be invoked to record how well the previous
  hypothesis performed. This recorder closure in turn returns a new
  closure of the type that this function returns.

  Arguments:

  :vars - the variables to use when generating hypotheses
  :max-constant - the greatest natural number to use as a constant in a
                  hypothesis
  :seen - a set of hypotheses bodies to keep track of to avoid repetitions
  :results - a map containing the body of a hypothesis and the ratio of valid
             positive tests passed and negative tests passed.
  :queue - already generated hypotheses to test next in that order
  :"
  [&{:keys [vars max-constant seen results queue]
     :or {seen #{} results [] queue []}}]
  (let [[queue results seen] (if (empty? queue)
                               (let [queue'
                                     (next-batch (first results) seen vars max-constant)]
                                 [new-queue
                                  (rest results)
                                  (into seen queue')])
                               [queue results seen])
        hypoth (first queue)]
    (fn []
      [hypoth (fn [ratio-valids ratio-invalids]
                (hypotheses :vars vars
                            :max-constant max-constant
                            :seen seen
                            :results (conj results {:body (body hypoth)
                                                    :ratio-valids ratio-valids
                                                    :ratio-invalids ratio-invalids})
                            :queue (rest queue)))])))
