(ns advent-of-code-2024.04.soln
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [clojure.set :as set]
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

(defn re-indexes [re s]
  (let [matcher (re-matcher re s)]
    (take-while some?
                (repeatedly
                 #(when (re-find matcher) (. matcher start))))))

(defn major-diagonal->coords [r w]
  ;; r is between -(size-1) and size-1 inclusive
  ;; w is between 0 and size-1 inclusive
  (let [c1 w c2 (+ w (abs r))]
    (case (math/signum r)
      0.0 [c1 c2]
      -1.0 [c2 c1]
      1.0 [c1 c2])))

(defn minor-diagonal->coords [r w size]
  (let [[x y] (major-diagonal->coords r w)]
    [x (- size y 1)]))

(defn diagonal-matches [str-mtx conversion-fn]
  ;; continuing to assume the word search is square
  (let [size (/ (+ (count str-mtx) 1) 2)]
    (->> str-mtx
         (map #(concat
                (re-indexes #"MAS" %)
                (re-indexes #"SAM" %)))
         (map-indexed
          (fn [idx data]
            (let [r (- size idx 1)]
              ;; add 1 bc we want the index of the 'A', not the beginning of the match
              (map #(conversion-fn r (+ % 1)) data))))
          ;; can't use flatten bc we want to keep the inner-most level of nesting
         (reduce #(concat %1 %2) [])
         set)))

(defn solve-pt2 [input]
  (let [input-matrix (matrix/array (map vec input))
        size (first (matrix/shape input-matrix))
        pipeline-fn #(-> input-matrix
                         %1
                         stringify-matrix
                         (diagonal-matches %2))]
    (count
     (set/intersection
      (pipeline-fn major-diagonals major-diagonal->coords)
      (pipeline-fn minor-diagonals #(minor-diagonal->coords %1 %2 size))))))

(def input (parse-input (slurp "src/advent_of_code_2024/04/input.txt")))

{:pt1-soln (solve-pt1 input)
 :pt2-soln (solve-pt2 input)}
;; => {:pt1-soln 2507, :pt2-soln 1969}
