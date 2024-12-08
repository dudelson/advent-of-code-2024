(ns advent-of-code-2024.03.journal
  (:require [clojure.string :as str]))

;; ===================================== PART 1 =====================================
;; The shopkeeper turns to you. "Any chance you can see why our computers are having issues again?"
;;
;; The computer appears to be trying to run a program, but its memory (your puzzle input) is corrupted. All of the instructions have been jumbled up!
;;
;; It seems like the goal of the program is just to multiply some numbers. It does that with instructions like mul(X,Y), where X and Y are each 1-3 digit numbers. For instance, mul(44,46) multiplies 44 by 46 to get a result of 2024. Similarly, mul(123,4) would multiply 123 by 4.
;;
;; However, because the program's memory has been corrupted, there are also many invalid characters that should be ignored, even if they look like part of a mul instruction. Sequences like mul(4*, mul(6,9!, ?(12,34), or mul ( 2 , 4 ) do nothing.))
;;
;; For example, consider the following section of corrupted memory:
;;
;; xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64then(mul(11,8)mul(8,5)))
;;
;; Only the four highlighted sections are real mul instructions. Adding up the result of each instruction produces 161 (2*4 + 5*5 + 11*8 + 8*5).
;;
;; Scan the corrupted memory for uncorrupted mul instructions. What do you get if you add up all of the results of the multiplications?

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  ;; the input is just a string, so no parsing function is needed in this case
  (def test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64then(mul(11,8)mul(8,5)))")
  ;; so we just want to find all instances of a regex in the string, and then reduce over that
  ;; list where we pull the numbers out of each match and multiply them (and add that to the total).
  (re-seq #"mul\((\d+),(\d+)\)" test-input)
  ;; => (["mul(2,4)" "2" "4"]
  ;;     ["mul(5,5)" "5" "5"]
  ;;     ["mul(11,8)" "11" "8"]
  ;;     ["mul(8,5)" "8" "5"])

  ;; perfect. and now we just reduce over that and add them up:
  ;; ok actually this is a bit involved so let me build it up a bit.
  ;; we have one match and we want to multiply the values:
  (def match (first (re-seq #"mul\((\d+),(\d+)\)" test-input)))
  (map #(Integer/parseInt %) (rest match))
  (apply * (map #(Integer/parseInt %) (rest match)))
  (reduce
   (fn [acc val]
     (+ acc
        (apply * (map #(Integer/parseInt %) (rest val)))))
   0
   (re-seq #"mul\((\d+),(\d+)\)" test-input))
  ;; => 161
  ;; there we go!
  ;; now i'll put it in a function and try it on the real input:
  (defn solve-pt1 [input]
    (reduce
     (fn [acc val]
       (+ acc
          (apply * (map #(Integer/parseInt %) (rest val)))))
     0
     (re-seq #"mul\((\d+),(\d+)\)" input)))

  (def input (str/trim (slurp "src/advent_of_code_2024/03/input.txt")))
  (solve-pt1 input)
  ;; => 157621318
  ;; accepted!
  ())

(def input (str/trim (slurp "src/advent_of_code_2024/03/input.txt")))

;; ===================================== PART 2 =====================================
;; As you scan through the corrupted memory, you notice that some of the conditional statements are also still intact. If you handle some of the uncorrupted conditional statements in the program, you might be able to get an even more accurate result.
;;
;; There are two new instructions you'll need to handle:
;;
;;     The do() instruction enables future mul instructions.
;;     The don't() instruction disables future mul instructions.
;;
;; Only the most recent do() or don't() instruction applies. At the beginning of the program, mul instructions are enabled.
;;
;; For example:
;;
;; xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64(mul(11,8)undo()?mul(8,5)))
;;
;; This corrupted memory is similar to the example from before, but this time the mul(5,5) and mul(11,8) instructions are disabled because there is a don't() instruction before them. The other mul instructions function normally, including the one at the end that gets re-enabled by a do() instruction.
;;
;; This time, the sum of the results is 48 (2*4 + 8*5).
;;
;; Handle the new instructions; what do you get if you add up all of the results of just the enabled multiplications?

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  ;; i think all we need to do here is turn the reduce result into a map
  ;; that contains a boolean indicating whether we're enabled or not, and
  ;; then we can conditionally add things to the accumulator as we iterate
  ;; through the results
  (def test-input "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64(mul(11,8)undo()?mul(8,5)))")
  ;; here's the soln from pt 1:
  (defn solve-pt1 [input]
    (reduce
     (fn [acc val]
       (+ acc
          (apply * (map #(Integer/parseInt %) (rest val)))))
     0
     (re-seq #"mul\((\d+),(\d+)\)" input)))
  ;; first i have to expand the regex to find these new commands:
  (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" test-input)
  ;; => (["mul(2,4)" "2" "4"]
  ;;     ["mul(5,5)" "5" "5"]
  ;;     ["mul(11,8)" "11" "8"]
  ;;     ["mul(8,5)" "8" "5"])
  ;; that didn't work. lemme fuddle around on regex101 for a sec.
  ;; oh wait i just forgot to eval the new test input. The actual result
  ;; is correct:
  ;; => (["mul(2,4)" "2" "4"]
  ;;     ["don't()" nil nil]
  ;;     ["mul(5,5)" "5" "5"]
  ;;     ["mul(11,8)" "11" "8"]
  ;;     ["do()" nil nil]
  ;;     ["mul(8,5)" "8" "5"])

  ;; here's a form to basically do a switch statement on the value of the
  ;; current match
  ;; research indicates that the right macro for this use-case is (cond).
  ;; and clojure.string does have a starts-with? fn, which is perfect for
  ;; this use-case.
  (map
   (fn [match]
     (cond
       (str/starts-with? (first match) "mul") "MUL"
       (str/starts-with? (first match) "do(") "DO"
       (str/starts-with? (first match) "don't") "DON'T"
       :else "???"))
   (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" test-input))
  ;; => ("MUL" "DON'T" "MUL" "MUL" "DO" "MUL")
  ;; and actually it looks like we can make it slightly more concise using
  ;; (condp) instead of (cond):
  (map
   (fn [match]
     (condp #(str/starts-with? %2 %1) (first match)
       "mul" "MUL"
       "do(" "DO"
       "don't" "DON'T"
       "???"))
   (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" test-input))
  ;; => ("MUL" "DON'T" "MUL" "MUL" "DO" "MUL")

  ;; so now we just modify the soln from part 1 to use this
  (defn solve-pt2 [input]
    (->> input
         (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)")
         (reduce
          (fn [acc val]
            (condp #(str/starts-with? %2 %1) (first val)
              "do(" (assoc acc :mul? true)
              "don't" (assoc acc :mul? false)
              "mul" (if (:mul? acc)
                      (assoc
                       acc :sum
                       (+ (:sum acc)
                          (apply * (map #(Integer/parseInt %) (rest val)))))
                      acc)
              acc))
          {:sum 0 :mul? true})
         :sum))
  (solve-pt2 test-input)
  ;; => 48
  ;; daaamn that's right!!
  ;; real input:
  (solve-pt2 input)
  ;; => 79845780
  ;; accepted! =D
  ())
