(ns guess.hypoth
  (:require [guess.synth :as synth]))

(defn body
  "Return the body of a hypothesis."
  [hypoth]
  (first (nnext hypoth)))

(defn builder [seq]
  (fn []
    [(when-let [it (first seq)]
       it)
     (fn [x y] (builder (rest seq)))]))

(defn hypotheses [& {:keys [vars max-constant]}]
  (let [all (synth/candidates :vars vars :max-constant max-constant)]
    (builder all)))
