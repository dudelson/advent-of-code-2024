(ns advent-of-code-2024.07.journal
  (:require [clojure.string :as str]))

;; PART ONE
;;
;; When you go to cross the bridge, you notice a group of engineers trying to repair it. (Apparently, it breaks pretty frequently.) You won't be able to cross until it's fixed.
;;
;; You ask how long it'll take; the engineers tell you that it only needs final calibrations, but some young elephants were playing nearby and stole all the operators from their calibration equations! They could finish the calibrations if only someone could determine which test values could possibly be produced by placing any combination of operators into their calibration equations (your puzzle input).
;;
;; For example:
;;
;; 190: 10 19
;; 3267: 81 40 27
;; 83: 17 5
;; 156: 15 6
;; 7290: 6 8 6 15
;; 161011: 16 10 13
;; 192: 17 8 14
;; 21037: 9 7 18 13
;; 292: 11 6 16 20
;;
;; Each line represents a single equation. The test value appears before the colon on each line; it is your job to determine whether the remaining numbers can be combined with operators to produce the test value.
;;
;; Operators are always evaluated left-to-right, not according to precedence rules. Furthermore, numbers in the equations cannot be rearranged. Glancing into the jungle, you can see elephants holding two different types of operators: add (+) and multiply (*).
;;
;; Only three of the above equations can be made true by inserting operators:
;;
;;     190: 10 19 has only one position that accepts an operator: between 10 and 19. Choosing + would give 29, but choosing * would give the test value (10 * 19 = 190).
;;     3267: 81 40 27 has two positions for operators. Of the four possible configurations of the operators, two cause the right side to match the test value: 81 + 40 * 27 and 81 * 40 + 27 both equal 3267 (when evaluated left-to-right)!
;;     292: 11 6 16 20 can be solved in exactly one way: 11 + 6 * 16 + 20.
;;
;; The engineers just need the total calibration result, which is the sum of the test values from just the equations that could possibly be true. In the above example, the sum of the test values for the three equations listed above is 3749.
;;
;; Determine which equations could possibly be true. What is their total calibration result?

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  ;; i'm going to brute force this and if it's too slow i'll go back to the drawing board

  ;; i'm using calva for this one since there have been a couple of problems with nvim+conjure
  ;; that i don't have the patience to look into right now
  ;; testing to make sure repl works:
  (+ 1 1)
  (->> (range 10) (map #(* % %)) reverse vec)
  ;; => [81 64 49 36 25 16 9 4 1 0]
  ;; great

  ;; parse input:
  (def test-input-raw "
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

  (re-matches #"(\d+): (.*)" "190: 10 19")
  (re-matches #"(\d+): (.*)" "21037: 9 7 18 13")
  (mapv #(Integer/parseInt %) (str/split "9 7 18 13" #" "))
  ;; returns vector where each element has shape [target nums-vec]
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
  (def test-input (parse-input test-input-raw))

  ;; generate all permutations of our two operators for a given length
  (format "%03s" (Integer/toBinaryString 2))
  (format "%%0%dd" 3)
  (let [n 3]
    (map #(format (format "%%0%dd" n) (Integer/toBinaryString %)) (range n)))
  ;; doing this with a string is becoming messy.
  ;; research shows me that there's a bit-test function i can use
  (apply * (repeat 4 2)) ;; => 16
  (defn op-combos [n]
    (map
     (fn [i]
       (map #(if (bit-test i %) + *) (range n)))
     ;; too lazy to import math/pow
     (range (apply * (repeat n 2)))))

  ;; brute-force solution (try every combo of ops)
  (let [target 3267
        nums [81 40 27]
        op-combos [[+ +] [+ *] [* +] [* *]]
        nums-pairwise (partition 2 1 nums)]
    (map
     (fn [ops]
       #_(apply str
                (mapcat vector ops (rest nums)))
       (reduce
        (fn [acc [op n]] (op acc n))
        (first nums)
        (map vector ops (rest nums))))
     op-combos))
  ;; => (148 3267 3267 87480)
  ;; this is correct bc the problem descr notes that for this input there are two ways to
  ;; produce the target value
  (defn can-produce-target? [target nums]
    (let [op-combos (op-combos (dec (count nums)))]
      (some
       (partial = target)
       (map
        (fn [ops]
          (reduce
           (fn [acc [op n]] (op acc n))
           (first nums)
           (map vector ops (rest nums))))
        op-combos))))
  (apply can-produce-target? (second test-input)) ;; => true
  (apply can-produce-target? (nth test-input 5)) ;; => nil

  (let [input test-input]
    (->> input
         (filter #(apply can-produce-target? %))
         (map first)
         (apply +)))
  ;; => 3749
  ;; ok, that's the correct answer for the test input. now for the real input:
  (defn solve-pt1 [input]
    (->> input
         (filter #(apply can-produce-target? %))
         (map first)
         (apply +)))
  (def input (parse-input (slurp "src/advent_of_code_2024/07/input.txt")))
  (solve-pt1 input)
  ;; okay so some of the target numbers in the test input are so big they have to be
  ;; parsed as longs. i've made that change above.
  (solve-pt1 input)
  ;; => 14711933466277
  ;; accepted!
  ())

(def test-input-raw "
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

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

(def test-input (parse-input test-input-raw))
(def input (parse-input (slurp "src/advent_of_code_2024/07/input.txt")))
;; ==================================================================================
;; PART TWO
;;
;; The engineers seem concerned; the total calibration result you gave them is nowhere close to being within safety tolerances. Just then, you spot your mistake: some well-hidden elephants are holding a third type of operator.
;;
;; The concatenation operator (||) combines the digits from its left and right inputs into a single number. For example, 12 || 345 would become 12345. All operators are still evaluated left-to-right.
;;
;; Now, apart from the three equations that could be made true using only addition and multiplication, the above example has three more equations that can be made true by inserting operators:
;;
;;     156: 15 6 can be made true through a single concatenation: 15 || 6 = 156.
;;     7290: 6 8 6 15 can be made true using 6 * 8 || 6 * 15.
;;     192: 17 8 14 can be made true using 17 || 8 + 14.
;;
;; Adding up all six test values (the three that could be made before using only + and * plus the new three that can now be made by also using ||) produces the new total calibration result of 11387.
#_{:clj-kondo/ignore [:redefined-var]}
(comment
  ;; I'm going to continue to try to brute-force and only go back to the drawing board if it's too slow

  ;; first we need to implement the new operator
  ;; we could make it any arity but i don't think that's going to be necessary for the problem,
  ;; so i'll just stick to 2 for now
  (defn concat-nums [n m]
    (let [ns (str n) ms (str m)]
      (Long/parseLong (str ns ms))))
  (concat-nums 1 2) ;; => 12
  (concat-nums 123 456) ;; => 123456
  (concat-nums 1234567890 1234567890) ;; => NumberFormatException
  ;; so we can't go too big or else we can't even parse a long; hopefully that doesn't screw things up
  ;; assuming the size of the numbers isn't a problem, the only other thing we have to do is update
  ;; the function that produces the operator combos, and then the rest actually stays the same
  (keyword +) ;; => nil
  (keyword '+) ;; => :+
  (keyword (quote concat-nums)) ;; => :concat-nums
  (let [m {+ *, * concat-nums, concat-nums +}]
    (get m +)) ;; => #function [clojure.core/*]
  ;; it seems like i can just use the functions as keys directly
  (let [ops [concat-nums * +]
        inc-op {+ *, * concat-nums, concat-nums +}]
    (inc-op (first ops))) ;; => #function [clojure.core/+]

  (let [v [1 2 3]]
    (v 2)
    #_(update v 1 #(* % %)))
  (let [v [+ + +]
        inc-op {+ *, * concat-nums, concat-nums +}]
    (update v 2 inc-op))
  (defn inc-ops [ops]
    (let [size (count ops)
          inc-op {+ *, * concat-nums, concat-nums +}]
      (loop [i 0, ops ops]
        (let [op (ops i)
              ops (update ops i inc-op)]
          (if (and (= op concat-nums) (< (inc i) size))
            (recur (inc i) ops)
            ops)))))
  
  (defn op-combos [n]
    (let [n-combos (apply * (repeat n 3))
          init (vec (repeat n +))]
      (take n-combos (iterate inc-ops init))))
  (op-combos 3)
  (op-combos 4)
  (let [n 3]
    (vec (repeat n +)))
  
  (defn solve-pt2 [input] (solve-pt1 input))
  (solve-pt2 test-input)
  ;; => 11387
  ;; that is correct for the test input
  ;; now let's see if it doesn't overflow for the real input:
  (solve-pt2 input)
  ;; => 286580387663654
  ;; accepted.
  ;; that one was scary bc it took several seconds to compute. thought i locked up my computer for a sec.
  ())