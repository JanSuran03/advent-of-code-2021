(ns advent-of-code-2021.task-4
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(def board-size 5)
(def total-boards 100)
(def empty-board (vec (repeat board-size (vec (repeat board-size false)))))
(def empty-boards (vec (repeat total-boards empty-board)))

(defn check-board-winning [board-data]
  (or (some #(every? true? %)
            board-data)
      (->> board-data (apply interleave)
           (partition 5)
           (some #(every? true? %)))))

(defn read-input
  "Returns a vector of:
   1) a map of key-value pairs where
      -- key = number
      -- value = vector of [board col row] paths to where that number can be found in all
         the boards.
   2) the numbers which will be called in the given order.
   3) the board (for final step of the task)"
  []
  (let [[numbers & boards] (-> "task-4-input.txt" slurp
                               str/split-lines)
        numbers (as-> numbers <>
                      (str/split <> #"\,")
                      (map read-string <>))
        boards (->> boards (filter first)
                    (mapv #(as-> % s
                                 (str/split s #"\ ")
                                 (filter first s)
                                 (mapv read-string s)))
                    (partition board-size)
                    (map vec))
        initial-numbers-map (->> (range 100)
                                 (map #(vector % []))
                                 (into {}))
        number-paths-map (loop [board 0
                                row 0
                                col 0
                                ret initial-numbers-map]
                           (cond (= board total-boards)
                                 ret

                                 (= row board-size)
                                 (recur (inc board)
                                        0
                                        col
                                        ret)

                                 (= col board-size)
                                 (recur board
                                        (inc row)
                                        0
                                        ret)

                                 :else
                                 (let [number (-> boards (nth board) (nth row) (nth col))]
                                   (recur board
                                          row
                                          (inc col)
                                          (update ret number conj [board row col])))))]
    [number-paths-map numbers boards]))

(defn get-maybe-winning-board [current-state board-indices]
  (loop [[current-board-index & remaining-boards] board-indices]
    (when current-board-index
      (let [board (nth current-state current-board-index)]
        (if (check-board-winning board)
          current-board-index
          (recur remaining-boards))))))

(defn solve-part-1 []
  (let [[number-paths-map numbers boards] (read-input)
        [winning-number winning-board passed-numbers boards]
        (loop [[current-number & remaining-numbers] numbers
               current-board empty-boards
               passed-numbers #{}]
          (let [need-to-be-marked-paths (get number-paths-map current-number)
                affected-boards (map first need-to-be-marked-paths)
                new-board (reduce (fn [board path-to-marked-cell]
                                    (assoc-in board path-to-marked-cell true))
                                  current-board
                                  need-to-be-marked-paths)]
            (Thread/sleep 100)
            (if-let [winning-board (get-maybe-winning-board new-board affected-boards)]
              [current-number winning-board passed-numbers boards]
              (recur remaining-numbers
                     new-board
                     (conj passed-numbers current-number)))))
        sum-of-unpassed-numbers (->> winning-board (nth boards)
                                     (apply concat)
                                     (remove passed-numbers)
                                     (apply +))]
    (* winning-number sum-of-unpassed-numbers)))