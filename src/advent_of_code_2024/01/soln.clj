(ns advent-of-code-2024.01.soln
  (:require [clojure.string :as str]))

(defn parse-input [raw-data]
  (->> raw-data
       str/trim
       str/split-lines
       (map
        (fn [s]
          (as-> s $
            (str/split $ #"   ")
            (map #(Integer/parseInt %) $))))
       (apply map vector)))

(def input (parse-input (slurp "src/advent_of_code_2024/01/input.txt")))

(defn part1 [input]
  (->> input
       (map sort)
       (apply map #(abs (- %1 %2)))
       (reduce +)))

(defn part2 [input]
  (let [left-list (first input)
        freq-table (frequencies (second input))]
    (->> left-list
         (map #(* % (get freq-table % 0)))
         (reduce +))))

{:part1-soln (part1 input)
 :part2-soln (part2 input)}
;; {:part1-soln 2756096, :part2-soln 23117829}
