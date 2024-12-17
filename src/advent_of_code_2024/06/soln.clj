(ns advent-of-code-2024.06.soln
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [raw-input]
  (->> raw-input
        str/trim
        str/split-lines
        (map vec)
        vec))

(def input (parse-input (slurp "src/advent_of_code_2024/06/input.txt")))

;; run the simulation until the guard hits the next obstacle
(defn simulation-step [terrain-map pos dir]
  (let [size (count terrain-map)  ;; assuming the terrain map is square
        iteration-fn (case dir
                        :north (fn [[x y]] (vector x (dec y)))
                        :east  (fn [[x y]] (vector (inc x) y))
                        :south (fn [[x y]] (vector x (inc y)))
                        :west  (fn [[x y]] (vector (dec x) y)))]
    (take-while
      (fn [[x y]] (and (not= (get-in terrain-map [y x]) \#) (<= -1 x size) (<= -1 y size)))
      (iterate iteration-fn pos))))

(defn solve-pt1 [terrain-map]
  (let [size (count terrain-map)  ;; assuming the terrain map is square
        guard-pos (first
                  (for [x (range size)
                        y (range size)
                        :when (= \^ (get-in terrain-map [y x]))]
                    [x y]))
        ;; assuming the guard always starts facing to the north
        guard-dir :north
        in-bounds? (fn [[x y]] (and (<= 0 x (dec size)) (<= 0 y (dec size))))]
    (->>
      (take-while
        #(in-bounds? (:guard-pos %))
        (iterate
          (fn [{:keys [guard-pos guard-dir]}]
            (let [cells-traversed (simulation-step terrain-map guard-pos guard-dir)
                  new-pos (last cells-traversed)]
              {:guard-pos new-pos
               :guard-dir (case guard-dir
                            :north :east
                            :east  :south
                            :south :west
                            :west  :north)
               :cells-traversed cells-traversed}))
          {:guard-pos guard-pos :guard-dir guard-dir :cells-traversed []}))
      vec
      (#(conj % {:cells-traversed
                 (butlast (simulation-step terrain-map (:guard-pos (last %)) (:guard-dir (last %))))
                 :guard-pos nil :guard-dir nil}))
      (map #(set (:cells-traversed %)))
      (apply set/union)
      count)))

;;(println "part 1 solution is: " (solve-pt1 input))
{:part1-soln (solve-pt1 input)}
;; => {:part1-soln 5153}

