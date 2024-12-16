(ns advent-of-code-2024.04.soln
  (:require [clojure.string :as str]
            [clojure.core.matrix :as matrix]))

(defn parse-input [raw-input]
  (-> raw-input
      str/trim
      str/split-lines))

(defn matrix->diagonals [input]
  (let [size (first (matrix/shape input))]
    (->> (range (- size) size)
         (map #(matrix/diagonal input %))
         (remove empty?))))

(def major-diagonals matrix->diagonals)
(defn minor-diagonals [input]
  (let [size (first (matrix/shape input))]
    (as-> input $
      (reduce
       #(apply matrix/swap-rows %1 %2)
       $
         ;; this math will work for both even and odd nums as long as it's floor division
       (map #(vector % (- size % 1)) (range (quot size 2))))
      (matrix->diagonals $))))

(defn generate-transformation [mtx n]
  (case n
    1 (matrix/transpose mtx)
    2 (major-diagonals mtx)
    3 (minor-diagonals mtx)
    mtx))

(def stringify-matrix (partial map #(apply str %)))

(defn solve-pt1 [input]
  (let [input-matrix (matrix/array (map vec input))]
    (->> (range 4)
         (map
          (fn [transformation-idx]
            (->> transformation-idx
                 (generate-transformation input-matrix)
                 stringify-matrix
                 (map #(vector
                        (count (re-seq #"XMAS" %))
                        (count (re-seq #"SAMX" %)))))))
         flatten
         (apply +))))

(def input (parse-input (slurp "src/advent_of_code_2024/04/input.txt")))

{:pt1-soln (solve-pt1 input)}
;; => {:pt1-soln 2507}
