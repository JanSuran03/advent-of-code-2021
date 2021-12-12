(ns advent-of-code-2021.task-2
  (:require [clojure.string :as str]))

(defn read-input []
  (->> "task-2-input.txt"
       slurp
       str/split-lines
       (map #(let [[direction value] (str/split % #"\ ")]
               [direction (read-string value)]))))

(defn solve-part-1 []
  (let [input (read-input)]
    (loop [horizontal 0
           vertical 0
           input input]
      (if (seq input)
        (let [[direction value] (first input)]
          (case direction
            "forward" (recur (+ horizontal value)
                             vertical
                             (next input))
            "down" (recur horizontal
                          (+ vertical value)
                          (next input))
            "up" (recur horizontal
                        (- vertical value)
                        (next input))))
        (* horizontal vertical)))))

(defn solve-part-2 []
  (let [input (read-input)]
    (loop [horizontal 0
           vertical 0
           aim 0
           input input]
      (if (seq input)
        (let [[direction value] (first input)]
          (case direction
            "forward" (recur (+ horizontal value)
                             (+ vertical (* aim value))
                             aim
                             (next input))
            "down" (recur horizontal
                          vertical
                          (+ aim value)
                          (next input))
            "up" (recur horizontal
                        vertical
                        (- aim value)
                        (next input))))
        (* horizontal vertical)))))