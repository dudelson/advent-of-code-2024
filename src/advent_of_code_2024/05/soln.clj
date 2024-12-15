(ns advent-of-code-2024.05.soln
  (:require [clojure.string :as str]))

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

(defn solve-pt2 [input]
  (->> (:updates input)
      ;; this part is the same as pt1, except we change `filter` to `remove`
       (remove
        (fn [update]
          (let [len (count update)]
            (every?
             (fn [[ni nj]]
               (not (contains?
                     (get-in input [:rules ni])
                     nj)))
             (for [i (range len)
                   j (range len)
                   :when (< i j)]
               [(nth update i) (nth update j)])))))
       (map
        (fn [update]
          (let [len (count update)]
            (reduce
             (fn [v [i j]]
               (let [ni (nth v i)
                     nj (nth v j)]
                 (if
                  (contains?
                    (get-in input [:rules ni])
                    nj)
                  (-> v
                      (assoc i nj)
                      (assoc j ni))
                  v)))
             update
             (for [i (range len)
                   j (range len)
                   :when (< i j)]
               [i j])))))
       (map
        (fn [update]
          (let [middle-idx (quot (count update) 2)]
            (->> middle-idx
                 (nth update)
                 Integer/parseInt))))
       (reduce +)))

(def input
  (-> "src/advent_of_code_2024/05/input.txt"
      slurp
      parse-input
      create-input-map))

{:part1-soln (solve-pt1 input)
 :part2-soln (solve-pt2 input)}
;; => {:part1-soln 5087, :part2-soln 4971}
