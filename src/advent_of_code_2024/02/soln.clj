(ns advent-of-code-2024.02.soln
  (:require [clojure.string :as str])
  (:require [clojure.math :as math]))

(defn parse-input [raw-data]
  (->> raw-data
       str/trim
       str/split-lines
       (map
        (fn [s]
          (as-> s $
            (str/split $ #" ")
            (map #(Integer/parseInt %) $))))))

(def input (parse-input (slurp "src/advent_of_code_2024/02/input.txt")))

(defn solve-pt1 [input]
  (->> input
       (map
        (fn [report]
          (and (or (apply > report) (apply < report))
               (->> report
                    (partition 2 1)
                    (map #(abs (reduce - %)))
                    (map #(and (>= % 1) (<= % 3)))
                    (reduce #(and %1 %2))))))
       (filter true?)
       count))

{:part1-soln (solve-pt1 input)}
