(ns advent-of-code-2021.task-3
  (:require [clojure.string :as str]))

(defn read-input
  "Returns a sequence of: column-1, column-2, column-3, ..., column-n"
  []
  (as-> "task-3-input.txt" <>
        (slurp <>)
        (str/split-lines <>)
        (let [len (count <>)]
          (->> <> (apply interleave)
               (partition len)))))

(defn bin->dec [bits-as-seq]
  (loop [pow 1
         ret 0
         remaining-bits (reverse bits-as-seq)]
    (if (seq remaining-bits)
      (recur (* pow 2)
             (+ ret (* pow (first remaining-bits)))
             (next remaining-bits))
      ret)))

(defn solve-part-1 []
  (let [input (read-input)
        zero-or-one? (fn [coll]
                       (loop [one-dominates 0
                              coll coll]
                         (if (seq coll)
                           (case (first coll)
                             \1 (recur (inc one-dominates)
                                       (next coll))
                             (recur (dec one-dominates)
                                    (next coll)))
                           (if (pos? one-dominates)
                             1
                             0))))
        gamma (->> input (map zero-or-one?))
        epsilon (->> gamma (map #(bit-flip % 0)))]
    (* (bin->dec gamma) (bin->dec epsilon))))