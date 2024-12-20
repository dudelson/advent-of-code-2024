(ns advent-of-code-2024.08.journal
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;; PART ONE
;;
;; You find yourselves on the roof of a top-secret Easter Bunny installation.
;;
;; While The Historians do their thing, you take a look at the familiar huge antenna. Much to your surprise, it seems to have been reconfigured to emit a signal that makes people 0.1% more likely to buy Easter Bunny brand Imitation Mediocre Chocolate as a Christmas gift! Unthinkable!
;;
;; Scanning across the city, you find that there are actually many such antennas. Each antenna is tuned to a specific frequency indicated by a single lowercase letter, uppercase letter, or digit. You create a map (your puzzle input) of these antennas. For example:
;;
;; ............
;; ........0...
;; .....0......
;; .......0....
;; ....0.......
;; ......A.....
;; ............
;; ............
;; ........A...
;; .........A..
;; ............
;; ............
;;
;; The signal only applies its nefarious effect at specific antinodes based on the resonant frequencies of the antennas. In particular, an antinode occurs at any point that is perfectly in line with two antennas of the same frequency - but only when one of the antennas is twice as far away as the other. This means that for any pair of antennas with the same frequency, there are two antinodes, one on either side of them.
;;
;; So, for these two antennas with frequency a, they create the two antinodes marked with #:
;;
;; ..........
;; ...#......
;; ..........
;; ....a.....
;; ..........
;; .....a....
;; ..........
;; ......#...
;; ..........
;; ..........
;;
;; Adding a third antenna with the same frequency creates several more antinodes. It would ideally add four antinodes, but two are off the right side of the map, so instead it adds only two:
;;
;; ..........
;; ...#......
;; #.........
;; ....a.....
;; ........a.
;; .....a....
;; ..#.......
;; ......#...
;; ..........
;; ..........
;;
;; Antennas with different frequencies don't create antinodes; A and a count as different frequencies. However, antinodes can occur at locations that contain antennas. In this diagram, the lone antenna with frequency capital A creates no antinodes but has a lowercase-a-frequency antinode at its location:
;;
;; ..........
;; ...#......
;; #.........
;; ....a.....
;; ........a.
;; .....a....
;; ..#.......
;; ......A...
;; ..........
;; ..........
;;
;; The first example has antennas with two different frequencies, so the antinodes they create look like this, plus an antinode overlapping the topmost A-frequency antenna:
;;
;; ......#....#
;; ...#....0...
;; ....#0....#.
;; ..#....0....
;; ....0....#..
;; .#....A.....
;; ...#........
;; #......#....
;; ........A...
;; .........A..
;; ..........#.
;; ..........#.
;;
;; Because the topmost A-frequency antenna overlaps with a 0-frequency antinode, there are 14 total unique locations that contain an antinode within the bounds of the map.
;;
;; Calculate the impact of the signal. How many unique locations within the bounds of the map contain an antinode?

(def test-input-raw "
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
")

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  ;; parse input
  (defn parse-input [input]
    (->> input
         str/trim
         str/split-lines
         (mapv vec)))
  (def test-input (parse-input test-input-raw))

  ;; going to continue to assume that the input grid is square, since that held true
  ;; for prior puzzles
  ;;
  ;; build antenna location map
  (reduce-kv #(conj %1 [%2 %3]) [] [1 2 3 4]) ;; => [[0 1] [1 2] [2 3] [3 4]]
  ;; so i could either do two reduce-kv calls nested inside one another, or i
  ;; could pre-generate the indexes with a list comprehension
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
  (get-in test-input [4 5])
  (antenna-location-map test-input)

  ;; compute antinodes
  (into #{} (filter #(< % 10) [0 1 14 5 15 1]))
  (reduce #(conj %1 %2) [] {:a 1 :b 2})
  (combo/selections [1 2] 3)
  (let [input test-input]
    (let [size (count input)]
      (map
       (fn [coords]
         (map
          (fn [[[xa ya] [xb yb]]]
            (let [dx (- xb xa)
                  dy (- yb ya)]
              [[(- xa dx) (- ya dy)] [(+ xb dx) (+ yb dy)]]))
          (combo/combinations coords 2)))
       (vals (antenna-location-map input)))))

  (let [input test-input]
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
  ;; => 14
  ;; okay let's try it on the actual input
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
  (def input (parse-input (slurp "src/advent_of_code_2024/08/input.txt")))
  (solve-pt1 input)
  ;; => 357
  ;; accepted
  ;;
  ;; this soln could definitely be more concise. E.g. if it were all one function i wouldn't
  ;; have to redefine some of the let bindings multiple times, and i could combine some of the
  ;; loops into one. but this is good enough for now. I'll let cleaning it up be an exercise for later.
  ;; in particular, it isn't actually necessary to build a map indexed on the type of antenna, since
  ;; i only use this once and just end up calling vals on it immediately anyway. As long as the
  ;; positions are grouped by type going into the next stage of the pipeline, that's sufficient.
  ())

(defn parse-input [input]
  (->> input
       str/trim
       str/split-lines
       (mapv vec)))

(def test-input (parse-input test-input-raw))

(def input (parse-input (slurp "src/advent_of_code_2024/08/input.txt")))

;; ==============================================================================================
;; PART TWO
;;
;; Watching over your shoulder as you work, one of The Historians asks if you took the effects of resonant harmonics into your calculations.
;;
;; Whoops!
;;
;; After updating your model, it turns out that an antinode occurs at any grid position exactly in line with at least two antennas of the same frequency, regardless of distance. This means that some of the new antinodes will occur at the position of each antenna (unless that antenna is the only one of its frequency).
;;
;; So, these three T-frequency antennas now create many antinodes:
;;
;; T....#....
;; ...T......
;; .T....#...
;; .........#
;; ..#.......
;; ..........
;; ...#......
;; ..........
;; ....#.....
;; ..........
;;
;; In fact, the three T-frequency antennas are all exactly in line with two antennas, so they are all also antinodes! This brings the total number of antinodes in the above example to 9.
;;
;; The original example now has 34 antinodes, including the antinodes that appear on every antenna:
;;
;; ##....#....#
;; .#.#....0...
;; ..#.#0....#.
;; ..##...0....
;; ....0....#..
;; .#...#A....#
;; ...#..#.....
;; #....#.#....
;; ..#.....A...
;; ....#....A..
;; .#........#.
;; ...#......##
;;
;; Calculate the impact of the signal using this updated model. How many unique locations within the bounds of the map contain an antinode?

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  ;; so we just update the soln for part 1 to produce all the new antinodes
  ;; there's probably a better way to do this, but i'm tired so i'm just going to use loop/recur
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
  ;; => 34
  ;; which is correct for the test input. Let's try it on the actual input:
  (solve-pt2 input)
  ;; => 1266
  ;; accepted. wow i think that's record time for any of the parts of any of these puzzles.
  ;; only took like 5 mins.
  ())
