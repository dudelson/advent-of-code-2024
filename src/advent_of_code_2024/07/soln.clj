(ns advent-of-code-2024.07.soln
  (:require
   [clojure.string :as str]))

(defn parse-input [input-raw]
  (->> input-raw
       str/trim
       str/split-lines
       (map
        (fn [line]
          (let [[_ target-raw nums-raw] (re-matches #"(\d+): (.*)" line)
                target (Long/parseLong target-raw)
                nums (mapv #(Long/parseLong %) (str/split nums-raw #" "))]
            [target, nums])))))

(defn op-combos-pt1 [n]
  (map
   (fn [i]
     (map #(if (bit-test i %) + *) (range n)))
   ;; too lazy to import math/pow
   (range (apply * (repeat n 2)))))

(defn can-produce-target? [combo-fn target nums]
  (let [op-combos (combo-fn (dec (count nums)))]
    (some
     (partial = target)
     (map
      (fn [ops]
        (reduce
         (fn [acc [op n]] (op acc n))
         (first nums)
         (map vector ops (rest nums))))
      op-combos))))

(defn solve-pt1 [input]
  (->> input
       (filter #(apply can-produce-target? op-combos-pt1 %))
       (map first)
       (apply +)))

(defn concat-nums [n m]
  (let [ns (str n) ms (str m)]
    (Long/parseLong (str ns ms))))

(defn inc-ops [ops]
  (let [size (count ops)
        inc-op {+ *, * concat-nums, concat-nums +}]
    (loop [i 0, ops ops]
      (let [op (ops i)
            ops (update ops i inc-op)]
        (if (and (= op concat-nums) (< (inc i) size))
          (recur (inc i) ops)
          ops)))))

(defn op-combos-pt2 [n]
  (let [n-combos (apply * (repeat n 3))
        init (vec (repeat n +))]
    (take n-combos (iterate inc-ops init))))

(defn solve-pt2 [input]
  (->> input
       (filter #(apply can-produce-target? op-combos-pt2 %))
       (map first)
       (apply +)))

(def input (parse-input (slurp "src/advent_of_code_2024/07/input.txt")))
{:pt1-soln (solve-pt1 input)
 :pt2-soln (solve-pt2 input)}
;; => {:pt1-soln 14711933466277 :pt2-soln 286580387663654}