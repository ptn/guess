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
  (synth/synth-one (args hypoth) (synth/negate (body hypoth))))

(defn next-batch
  "Generate next batch of hypotheses to test."
  [seen result vars max-constant]
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
  closure of the type that this function returns."
  [&{:keys [vars max-constant seen results queue]
     :or {seen [] results [] queue []}}]
  (let [[queue results seen] (if (empty? queue)
                               (let [new-queue
                                     (next-batch seen (first results) vars max-constant)]
                                 [new-queue
                                  (rest results)
                                  (into seen new-queue)])
                               [queue results seen])
        hypoth (first queue)]
    (fn []
      [hypoth (fn [ratio-valids ratio-invalids]
                (hypotheses :vars vars
                            :max-constant max-constant
                            :seen seen
                            :results (conj results {:hypoth hypoth
                                                    :ratio-valids ratio-valids
                                                    :ratio-invalids ratio-invalids})
                            :queue (rest queue)))])))
