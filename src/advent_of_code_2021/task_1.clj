(ns advent-of-code-2021.task-1
  (:require [clojure.string :as str]))

(defn process-input []
  (->> "task-1-input.txt" slurp str/split-lines
       (map read-string)))

(defn shift-by [n coll]
  (loop [coll coll
         n n]
    (if (pos? n)
      (recur (cons ##Inf coll)
             (dec n))
      coll)))

(defn solve-part-1
  "How many measurements are greater than the previous one (1 before)?"
  []
  (let [input (process-input)]
    (->> (map > input (shift-by 1 input))
         (filter true?)
         count)))

(defn solve-part-2
  "If we took trinities, how many of them are larger than the sum of the previous trinity?
  (B + C + D) > (A + B + C)
  D > A
  A + 3 > 3
  =>> How many measurements are greater than the one 3 places before this one?"
  []
  (let [input (process-input)]
    (->> (map > input (shift-by 3 input))
         (filter true?)
         count)))
