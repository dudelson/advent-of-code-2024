(ns advent-of-code-2024.06.journal
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; PART ONE
;;
;; The Historians use their fancy device again, this time to whisk you all away to the North Pole prototype suit manufacturing lab... in the year 1518! It turns out that having direct access to history is very convenient for a group of historians.
;;
;; You still have to be careful of time paradoxes, and so it will be important to avoid anyone from 1518 while The Historians search for the Chief. Unfortunately, a single guard is patrolling this part of the lab.
;;
;; Maybe you can work out where the guard will go ahead of time so that The Historians can search safely?
;;
;; You start by making a map (your puzzle input) of the situation. For example:
;;
;; ....#.....
;; .........#
;; ..........
;; ..#.......
;; .......#..
;; ..........
;; .#..^.....
;; ........#.
;; #.........
;; ......#...
;;
;; The map shows the current position of the guard with ^ (to indicate the guard is currently facing up from the perspective of the map). Any obstructions - crates, desks, alchemical reactors, etc. - are shown as #.
;;
;; Lab guards in 1518 follow a very strict patrol protocol which involves repeatedly following these steps:
;;
;;     If there is something directly in front of you, turn right 90 degrees.
;;     Otherwise, take a step forward.
;;
;; Following the above protocol, the guard moves up several times until she reaches an obstacle (in this case, a pile of failed suit prototypes):
;;
;; ....#.....
;; ....^....#
;; ..........
;; ..#.......
;; .......#..
;; ..........
;; .#........
;; ........#.
;; #.........
;; ......#...
;;
;; Because there is now an obstacle in front of the guard, she turns right before continuing straight in her new facing direction:
;;
;; ....#.....
;; ........>#
;; ..........
;; ..#.......
;; .......#..
;; ..........
;; .#........
;; ........#.
;; #.........
;; ......#...
;;
;; Reaching another obstacle (a spool of several very long polymers), she turns right again and continues downward:
;;
;; ....#.....
;; .........#
;; ..........
;; ..#.......
;; .......#..
;; ..........
;; .#......v.
;; ........#.
;; #.........
;; ......#...
;;
;; This process continues for a while, but the guard eventually leaves the mapped area (after walking past a tank of universal solvent):
;;
;; ....#.....
;; .........#
;; ..........
;; ..#.......
;; .......#..
;; ..........
;; .#........
;; ........#.
;; #.........
;; ......#v..
;;
;; By predicting the guard's route, you can determine which specific positions in the lab will be in the patrol path. Including the guard's starting position, the positions visited by the guard before leaving the area are marked with an X:
;;
;; ....#.....
;; ....XXXXX#
;; ....X...X.
;; ..#.X...X.
;; ..XXXXX#X.
;; ..X.X.X.X.
;; .#XXXXXXX.
;; .XXXXXXX#.
;; #XXXXXXX..
;; ......#X..
;;
;; In this example, the guard will visit 41 distinct positions on your map.
;;
;; Predict the path of the guard. How many distinct positions will the guard visit before leaving the mapped area?

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  (def test-input-raw "
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
    ")

  (defn parse-input [raw-input]
    (->> raw-input
         str/trim
         str/split-lines
         (map vec)
         vec))

  (def test-input (parse-input test-input-raw))

  (let [v [[1 2 3] [4 5 6] [7 8 9]]]
    (get-in v [1 2])) ;; => 6
  (get-in test-input [4 6]) ;; => nil
  (get test-input 0)

  (map #(<= -1 % 10) [4 6])

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

  (simulation-step test-input [4 6] :north)

  (for [x (range (count test-input))
        y (range (count test-input))
        :when (= \^ (get-in test-input [y x]))]
    [x y])
  (first nil)

  (conj [1 2 3] 4)
  (#(+ % 1) 4)
  (assoc {} :a 1 :b 2)

  (let [terrain-map test-input
        ;; assuming the terrain map is square
        size (count terrain-map)
        guard-pos (first
                   (for [x (range size)
                         y (range size)
                         :when (= \^ (get-in terrain-map [y x]))]
                     [x y]))
        ;; assuming the guard always starts facing to the north
        guard-dir :north
        in-bounds? (fn [[x y]] (and (<= 0 x (dec size)) (<= 0 y (dec size))))]
    (let [cells-traversed (simulation-step terrain-map guard-pos guard-dir)
          new-pos (last cells-traversed)]
      {:guard-pos new-pos
       :guard-dir (case guard-dir
                    :north :east
                    :east  :south
                    :south :west
                    :west  :north)
       :cells-traversed cells-traversed}))

  (let [terrain-map test-input
        ;; assuming the terrain map is square
        size (count terrain-map)
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
     (#(conj %
             (assoc {}
                    :cells-traversed
                    (butlast (simulation-step terrain-map (:guard-pos (last %)) (:guard-dir (last %))))
                    :guard-pos nil :guard-dir nil)))
     (map #(set (:cells-traversed %)))
     (reduce set/union #{})
     count))
  ;; => 41
  ;; okay that's correct for the test input, time to put this all in a function
  ;; and try it on the actual input
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
        (#(conj %
                (assoc {}
                        :cells-traversed
                        (butlast (simulation-step terrain-map (:guard-pos (last %)) (:guard-dir (last %))))
                        :guard-pos nil :guard-dir nil)))
        (map #(set (:cells-traversed %)))
        (reduce set/union #{})
        count)))

  (def input (parse-input (slurp "src/advent_of_code_2024/06/input.txt")))
  (solve-pt1 input)
  ())
