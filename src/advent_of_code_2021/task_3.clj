(ns advent-of-code-2021.task-3
  (:require [clojure.string :as str]))

(defn read-input []
  (->> "task-3-input.txt"
       slurp
       str/split-lines))

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
  (let [input (as-> (read-input) <>
                    (let [len (count <>)]
                      (->> <> (apply interleave)
                           (partition len))))
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

(defn return-0-or-1-whichever-dominates [str-seq n]
  (loop [one-dominates 0
         remaining str-seq]
    (if (seq remaining)
      (let [ch (.charAt ^CharSequence (first remaining) n)]
        (case ch
          \1 (recur (inc one-dominates)
                    (next remaining))
          (recur (dec one-dominates)
                 (next remaining))))
      (if (neg? one-dominates)
        \0
        \1))))

(defn filter-nth-equals-char [str-seq n ch]
  (filter (fn [^CharSequence s]
            (= (.charAt s n)
               ch))
          str-seq))

(defn partition-until-one-element-is-remaining [str-seq most-common?]
  (let [len (count (first str-seq))]
    (loop [str-seq str-seq
           i 0]
      (cond (= i len)
            (recur str-seq
                   0)

            (second str-seq)
            (let [char-dominates (return-0-or-1-whichever-dominates str-seq i)]
              (if (or (and (= char-dominates \1)
                           most-common?)
                      (and (= char-dominates \0)
                           (not most-common?)))
                (recur (filter-nth-equals-char str-seq i \1)
                       (inc i))
                (recur (filter-nth-equals-char str-seq i \0)
                       (inc i))))

            :else
            (as-> str-seq <>
                  (first <>)
                  (str/split <> #"")
                  (map read-string <>)
                  (bin->dec <>))))))

(defn solve-part-2 []
  (let [input (read-input)
        oxygen (partition-until-one-element-is-remaining input true)
        CO2 (partition-until-one-element-is-remaining input false)]
    (* oxygen CO2)))