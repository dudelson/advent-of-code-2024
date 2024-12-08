(ns advent-of-code-2024.03.soln
  (:require [clojure.string :as str]))

(def input (str/trim (slurp "src/advent_of_code_2024/03/input.txt")))

(defn solve-pt1 [input]
  (reduce
   (fn [acc val]
     (+ acc
        (apply * (map #(Integer/parseInt %) (rest val)))))
   0
   (re-seq #"mul\((\d+),(\d+)\)" input)))

(defn solve-pt2 [input]
  (->> input
       (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)")
       (reduce
        (fn [acc val]
          (condp #(str/starts-with? %2 %1) (first val)
            "do(" (assoc acc :mul? true)
            "don't" (assoc acc :mul? false)
            "mul" (if (:mul? acc)
                    (assoc
                     acc :sum
                     (+ (:sum acc)
                        (apply * (map #(Integer/parseInt %) (rest val)))))
                    acc)
            acc))
        {:sum 0 :mul? true})
       :sum))

{:part1-soln (solve-pt1 input)
 :part2-soln (solve-pt2 input)}
;; => {:part1-soln 157621318, :part2-soln 79845780}
