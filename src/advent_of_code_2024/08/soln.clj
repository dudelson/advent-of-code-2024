(ns advent-of-code-2024.08.soln
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn parse-input [input]
  (->> input
       str/trim
       str/split-lines
       (mapv vec)))

(defn antenna-location-map [input]
  (let [size (count input)]
    (reduce
     (fn [acc [i j]]
       (update acc (get-in input [j i]) #(conj % [i j])))
     {}
     (for [i (range size)
           j (range size)
           :when (not= \. (get-in input [j i]))]
       [i j]))))

(defn solve-pt1 [input]
  (let [antennas (antenna-location-map input)
        size (count input)]
    (->> (map
          (fn [coords]
            (mapcat
             (fn [[[xa ya] [xb yb]]]
               (let [dx (- xb xa)
                     dy (- yb ya)]
                 [[(- xa dx) (- ya dy)] [(+ xb dx) (+ yb dy)]]))
             (combo/combinations coords 2)))
          (vals (antenna-location-map input)))
         (apply concat)
         (filter (fn [[x y]] (and (<= 0 x (dec size)) (<= 0 y (dec size)))))
         (into #{})
         (count))))

(defn solve-pt2 [input]
  (let [antennas (antenna-location-map input)
        size (count input)
        in-bounds? (fn [[x y]] (and (<= 0 x (dec size)) (<= 0 y (dec size))))]
    (->> (map
          (fn [coords]
            (mapcat
             (fn [[[xa ya] [xb yb]]]
               (let [dx (- xb xa)
                     dy (- yb ya)]
                 (loop [n 0, acc []]
                   (let [pa [(- xa (* dx n)) (- ya (* dy n))]
                         pb [(+ xb (* dx n)) (+ yb (* dy n))]]
                     (if (or (in-bounds? pa) (in-bounds? pb))
                       (recur (inc n) (conj acc pa pb))
                       acc)))))
             (combo/combinations coords 2)))
          (vals (antenna-location-map input)))
         (apply concat)
         (filter (fn [[x y]] (and (<= 0 x (dec size)) (<= 0 y (dec size)))))
         (into #{})
         (count))))

(def input (parse-input (slurp "src/advent_of_code_2024/08/input.txt")))
{:pt1-soln (solve-pt1 input)
 :pt2-soln (solve-pt2 input)}
;; => {:pt1-soln 357, :pt2-soln 1266}
