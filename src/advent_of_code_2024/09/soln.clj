(ns advent-of-code-2024.09.soln
  (:require [clojure.string :as str]))

(defn parse-input [raw-input]
  (let [nums (map #(Integer/parseInt (str %)) (str/trim raw-input))
        file-sizes (take-nth 2 nums)
        free-sizes (concat (take-nth 2 (rest nums)) [0])]
    (->> (map vector file-sizes free-sizes)
         (map-indexed
          (fn [idx [file-size free-size]]
            (concat (repeat file-size idx)
                    (repeat free-size nil))))
         (flatten)
         (vec))))

(defn next-free [disk i]
  (loop [idx i]
    (if (and (< idx (count disk)) (not= nil (disk idx)))
      (recur (inc idx))
      (if (= idx (count disk)) nil idx))))

(defn prev-file [disk j]
  (loop [idx j]
    (if (and (>= idx 0) (= nil (disk idx)))
      (recur (dec idx))
      (if (< idx 0) nil idx))))

(defn compact-disk [disk]
  (let [swap-blocks (fn [v i j] (assoc v, i (v j), j (v i)))]
    (loop [i (next-free disk 0)
           j (prev-file disk (dec (count disk)))
           disk disk]
      (if (< i j)
        (let [disk (swap-blocks disk i j)]
          (recur
           (next-free disk i)
           (prev-file disk j)
           disk))
        disk))))

(defn solve-pt1 [input]
  (->> input
       (compact-disk)
       (take-while some?)
       (vec)
       (reduce-kv (fn [acc idx file-id] (+ acc (* idx file-id))) 0)))

(def input (parse-input (slurp "src/advent_of_code_2024/09/input.txt")))

{:pt1-soln (solve-pt1 input)}
;; => {:pt1-soln 6398608069280}
