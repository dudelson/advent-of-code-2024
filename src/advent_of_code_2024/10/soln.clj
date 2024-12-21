(ns advent-of-code-2024.10.soln
  (:require [clojure.string :as str]))

(defn parse-input [raw-input]
  (->> raw-input
       (str/trim)
       (str/split-lines)
       (mapv
        (fn [s] (mapv #(Integer/parseInt (str %)) (vec s))))))

(defn trailheads [topo-map]
  (let [size (count topo-map)]
    (for [i (range size)
          j (range size)
          :when (= (get-in topo-map [j i]) 0)]
      [i j])))

(defn in-bounds? [topo-map [x y]]
  (let [size (count topo-map)]
    (and (<= 0 x (dec size)) (<= 0 y (dec size)))))

(defn neighbors [topo-map [x y]]
  (filter (partial in-bounds? topo-map)
          [[(dec x) y] [x (inc y)]
           [(inc x) y] [x (dec y)]]))

(defn score-trailhead [topo-map trailhead]
  (loop [pos trailhead, to-visit [], visited #{}, score 0]
    (let [topo-height (get-in topo-map (reverse pos))
          inc-score #(if (= topo-height 9) (inc %) %)
          neighbors (neighbors topo-map pos)
          to-visit (apply conj to-visit
                          (filter
                           (fn [[x y]]
                             (and
                              (nil? (visited [x y]))
                              (= (get-in topo-map [y x]) (inc topo-height))))
                           neighbors))
          visited (conj visited pos)]
      (if (empty? to-visit)
        (inc-score score)
        (recur (first to-visit) (rest to-visit) visited (inc-score score))))))

(defn solve-pt1 [input]
  (->> input
       (trailheads)
       (map #(score-trailhead input %))
       (apply +)))

(def input (parse-input (slurp "src/advent_of_code_2024/10/input.txt")))

{:pt1-soln (solve-pt1 input)}
;; => {:pt1-soln 796}
