(ns advent-of-code-2024.05.soln
  (:require [clojure.string :as str]))

(def test-input-raw "
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
    ")

(defn parse-input [raw-data]
  (->> raw-data
       str/trim
       str/split-lines
       (split-with (partial not= ""))))

(defn create-input-map [[rules-raw updates-raw]]
  {:rules
   (reduce
    (fn [acc s]
      (let [[l,r] (str/split s #"\|")]
        (update acc r #(conj (or % #{}) l))))
    {}
    rules-raw)
   :updates
   (map #(str/split % #",") (rest updates-raw))})

(defn solve-pt1 [input]
  (->> (:updates input)
       (filter
        (fn [update]
          (let [len (count update)]
            (every?
             (fn [[ni nj]]
               (not (contains? (get-in input [:rules ni]) nj)))
             (for [i (range len)
                   j (range len)
                   :when (< i j)]
               [(nth update i) (nth update j)])))))
       (map
        (fn [update]
          (let [middle-idx (quot (count update) 2)]
            (->> middle-idx
                 (nth update)
                 Integer/parseInt))))
       (reduce +)))

(def test-input (create-input-map (parse-input test-input)))

(def input
  (-> "src/advent_of_code_2024/05/input.txt"
      slurp
      parse-input
      create-input-map))

{:part1-soln (solve-pt1 input)}
;; => {:part1-soln 5087}
