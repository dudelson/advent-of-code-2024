(ns advent-of-code-2024.02.journal
  (:require [clojure.string :as str])
  (:require [clojure.math :as math]))

;; ========================= PART 1 =========================
;;   Problem Description
;; ------------------------
;; While the Red-Nosed Reindeer nuclear fusion/fission plant appears to contain no sign of the Chief Historian, the engineers there run up to you as soon as they see you. Apparently, they still talk about the time Rudolph was saved through molecular synthesis from a single electron.)

;; They're quick to add that - since you're already here - they'd really appreciate your help analyzing some unusual data from the Red-Nosed reactor. You turn to check if The Historians are waiting for you, but they seem to have already divided into groups that are currently searching every corner of the facility. You offer to help with the unusual data.
;;
;; The unusual data (your puzzle input) consists of many reports, one report per line. Each report is a list of numbers called levels that are separated by spaces. For example:
;;
;; 7 6 4 2 1
;; 1 2 7 8 9
;; 9 7 6 2 1
;; 1 3 2 4 5
;; 8 6 4 4 1
;; 1 3 6 7 9
;;
;; This example data contains six reports each containing five levels.
;;
;; The engineers are trying to figure out which reports are safe. The Red-Nosed reactor safety systems can only tolerate levels that are either gradually increasing or gradually decreasing. So, a report only counts as safe if both of the following are true:
;;
;;     The levels are either all increasing or all decreasing.
;;     Any two adjacent levels differ by at least one and at most three.
;;
;; In the example above, the reports can be found safe or unsafe by checking those rules:
;;
;;     7 6 4 2 1: Safe because the levels are all decreasing by 1 or 2.
;;     1 2 7 8 9: Unsafe because 2 7 is an increase of 5.
;;     9 7 6 2 1: Unsafe because 6 2 is a decrease of 4.
;;     1 3 2 4 5: Unsafe because 1 3 is increasing but 3 2 is decreasing.
;;     8 6 4 4 1: Unsafe because 4 4 is neither an increase or a decrease.
;;     1 3 6 7 9: Safe because the levels are all increasing by 1, 2, or 3.
;;
;; So, in this example, 2 reports are safe.
;;
;; Analyze the unusual data from the engineers. How many reports are safe?

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  ;; so first we want to figure out how to parse the input
  ;; here's the raw test data:
  (def raw-test-input "
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
    ")

  ;; here's a function to parse that:
  (defn parse-input [raw-data]
    (->> raw-data
         str/trim
         str/split-lines
         (map
          (fn [s]
            (as-> s $
              (str/split $ #" ")
              (map #(Integer/parseInt %) $))))))
  ;; testing the function:
  (def test-input (parse-input raw-test-input))

  ;; okay and now we want to solve the puzzle
  ;;
  ;; so we ultimately need to output the number of reports that are safe
  ;; we can map each report to either true or false and then count the number of true values in that seq
  ;; in order to determine whether a report is safe, we take the elements pairwise and compute their difference
  ;; so we "map" from the original report seq to a seq of deltas, which will have length n-1
  ;; if all of the deltas have the same sign and have magnitude between 1 and 3, the report is safe
  ;;
  ;; also worth noting that it would be more efficient to break early on a report as soon as we find either
  ;; one of the two conditions violated, but this seems to imply an imperative approach and i'm not sure
  ;; how to do it elegantly in clojure.
  ;; i think in practice the performance won't be a problem since the actual input has only 1000 rows,
  ;; so maybe if i want to i can come back later and try to come up with a more efficient version as an exercise.

  ;; in order to work on this, i'm first going to extract an individual report we can operate on:
  (def test-report (first test-input))
  ;; => (7 6 4 2 1)

  ;; so first we "map" from the seq to the seq of deltas
  ;; the first thing we have to figure out is if there's an elegant way to take items pairwise
  ;; my research tells me that i think i can just do this:
  (partition 2 1 test-report)
  ;; => ((7 6) (6 4) (4 2) (2 1))

  ;; that's sick bc not only did it work perfectly first try, but it also means that it's already
  ;; changed the length of the collection from n to n-1 for me, so now i can use map instead of reduce
  ;; so now it's easy to map to deltas:
  (->> test-report
       (partition 2 1)
       (map #(reduce - %)))
  ;; => (1 2 2 1)

  ;; and finally, we reduce the list of deltas to a boolean which indicates whether all safety conditions
  ;; are met.
  ;; i am not sure how to test whether all elements of a collection meet some condition (there doesn't
  ;; appear to be a stdlib function that does that), but in the context of reduce, we could verify that
  ;; all elements have the same sign by multiplying them and verifying that the result is positive. The
  ;; only thing is we would want to keep this number small so it doesn't overflow.
  ;; we can't use mod bc that will often return 0:
  (mod 5 1) ;; => 0
  ;; but i think we can divide by the absolute value of the number
  ;; there is probably some bit-shift nonsense we could do that would actually be the simplest, but i'm
  ;; not sure off the top of my head what that would be
  ;; WAIT i found this function: https://clojuredocs.org/clojure.math/copy-sign
  (math/copy-sign 1 -45) ;; => -1.0
  (math/copy-sign 1 360) ;; => 1.0
  ;; this is unexpected:
  (math/copy-sign 1 0) ;; => 1.0
  (math/copy-sign -1 0) ;; => 1.0
  ;; wait so signum handles the zero case the way i want, and is also more straightforward.

  ;; ok wait this is all dumb, i found the function i want: it's called (every?).
  ;; actually i do want to combine every? with signum:
  (every? #(= 1.0 (math/signum %)) [1, 2, 2, 1]) ;; => true
  (every? #(= -1.0 (math/signum %)) [-1, -2, -2, -1]) ;; => true
  (every? #(= 1.0 (math/signum %)) [-1, -2, 2, 1]) ;; => false
  (every? #(= -1.0 (math/signum %)) [-1, -2, 2, 1]) ;; => false
  ;; there's probably still some bit-fu i can do to reduce a list to a single value which is positive
  ;; if all the signs were the same and negative otherwise, but i'll leave that as an optional exercise
  ;; for later.
  ;;
  ;; OOPS actually i forgot about pos? and neg?. So it's actually very concise:
  (defn all-same-sign? [coll]
    (or (every? pos? coll) (every? neg? coll)))
  (all-same-sign? [1, 2, 2, 1]) ;; => true
  (all-same-sign? [-1, -2, -2, -1]) ;; => true
  (all-same-sign? [1, 2, -2, -1]) ;; => false
  (all-same-sign? [1, 2, 2, 0]) ;; => false
  (all-same-sign? [-1, 0, -2, -1]) ;; => false

  ;; holy shit it gets even simpler. clojure is insane:
  ;; https://clojuredocs.org/clojure.core/%3E=
  ;; Returns non-nil if nums are in monotonically non-increasing order, otherwise false.
  ;;   user=> (>= 6 5 4 3 2)
  ;;   true
  (apply >= [7 6 4 2 1]) ;; => true
  (apply >= [8 6 4 4 1]) ;; => true
  ;; so actually i want > bc that last one was supposed to be false
  (apply > [7 6 4 2 1]) ;; => true
  (apply > [8 6 4 4 1]) ;; => false
  ;; that's fuckin nuts

  ;; and then to verify the magnitude condition, i guess we keep the list of deltas and we do this:
  (->> [1 2 2 1]
       (map #(and (>= % 1) (<= % 3)))
       (reduce #(and %1 %2)))
  ;; if I do just (reduce and) i get the error "can't take the value of a macro". there must be a really
  ;; simple way to reduce a list of booleans via either logical and or logical or. I would be surprised
  ;; if (reduce #(and %1 %2) coll) was the most concise way to do it. I guess this can also be an optional
  ;; exercise for later.
  ;;
  ;; oh actually it looks like you can do (every? true? coll):
  (every? true? [true true true]) ;; => true
  (every? true? [true false true]) ;; => false

  ;; ok so putting these two things together for my test report:
  (->> test-report
       (partition 2 1)
       (map #(reduce - %))
       (map #(and (>= % 1) (<= % 3)))
       (every? true?))
  ;; => true
  (and (or (apply > test-report) (apply < test-report))
       (->> test-report
            (partition 2 1)
            (map #(reduce - %))
            (map #(and (>= % 1) (<= % 3)))
            (reduce #(and %1 %2))))
  ;; => true

  ;; ok so now we can put this into a function which maps over all reports:
  ;; note that this is not the final solution bc we would have to count the number of true
  ;; entries in the computed seq.
  (defn solve-pt1 [input]
    (map
     (fn [report]
       (and (or (apply > report) (apply < report))
            (->> report
                 (partition 2 1)
                 (map #(reduce - %))
                 (map #(and (>= % 1) (<= % 3)))
                 (reduce #(and %1 %2)))))
     input))
  (solve-pt1 test-input) ;; => (true false false false false false)
  ;; to paste the results again:
  ;;
  ;;   In the example above, the reports can be found safe or unsafe by checking those rules:
  ;;
  ;;       7 6 4 2 1: Safe because the levels are all decreasing by 1 or 2.
  ;;       1 2 7 8 9: Unsafe because 2 7 is an increase of 5.
  ;;       9 7 6 2 1: Unsafe because 6 2 is a decrease of 4.
  ;;       1 3 2 4 5: Unsafe because 1 3 is increasing but 3 2 is decreasing.
  ;;       8 6 4 4 1: Unsafe because 4 4 is neither an increase or a decrease.
  ;;       1 3 6 7 9: Safe because the levels are all increasing by 1, 2, or 3.
  ;;
  ;;   So, in this example, 2 reports are safe.
  ;;
  ;; so the last one is not correct
  ;; i'm going to redefine my test report so we can look at that:
  (def test-report (last test-input))
  (and (or (apply > test-report) (apply < test-report))
       (->> test-report
            (partition 2 1)
            (map #(reduce - %))
            (map #(and (>= % 1) (<= % 3)))
            (reduce #(and %1 %2))))
  ;; => false
  (or (apply > test-report) (apply < test-report))
  ;; => true
  (->> test-report
       (partition 2 1)
       (map #(reduce - %))
       (map #(and (>= % 1) (<= % 3)))
       (reduce #(and %1 %2)))
  ;; => false
  (->> test-report
       (partition 2 1)
       (map #(reduce - %)))
  ;; => (-2 -3 -1 -2)
  ;; riiiight i need to use abs now that this pipeline is only for checking the magnitudes
  (defn solve-pt1 [input]
    (map
     (fn [report]
       (and (or (apply > report) (apply < report))
            (->> report
                 (partition 2 1)
                 (map #(abs (reduce - %)))
                 (map #(and (>= % 1) (<= % 3)))
                 (reduce #(and %1 %2)))))
     input))
  (solve-pt1 test-input) ;; => (true false false false false true)

  ;; sick. so now we just count how many true values we get:
  (defn solve-pt1 [input]
    (->> input
         (map
          (fn [report]
            (and (or (apply > report) (apply < report))
                 (->> report
                      (partition 2 1)
                      (map #(abs (reduce - %)))
                      (map #(and (>= % 1) (<= % 3)))
                      (reduce #(and %1 %2))))))
         (filter true?)
         count))
  (solve-pt1 test-input) ;; => 2

  ;; ok, that checks out. Let's try it on the big cheese:
  (def input (parse-input (slurp "src/advent_of_code_2024/02/input.txt")))
  (solve-pt1 input)
  ;; => 390
  ;; correct! =D
  ())

;; ----------------------------------------------------------------------
;;
;; aggregating values from pt 1 which will be needed for pt 2 outside of the comment block:
(def raw-test-input "
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
  ")

(defn parse-input [raw-data]
  (->> raw-data
       str/trim
       str/split-lines
       (map
        (fn [s]
          (as-> s $
            (str/split $ #" ")
            (map #(Integer/parseInt %) $))))))

(def test-input (parse-input raw-test-input))
(def input (parse-input (slurp "src/advent_of_code_2024/02/input.txt")))
;;
;; ----------------------------------------------------------------------

;; ========================= PART 2 =========================
;;   Problem Description
;; ------------------------
;; The engineers are surprised by the low number of safe reports until they realize they forgot to tell you about the Problem Dampener.
;;
;; The Problem Dampener is a reactor-mounted module that lets the reactor safety systems tolerate a single bad level in what would otherwise be a safe report. It's like the bad level never happened!
;;
;; Now, the same rules apply as before, except if removing a single level from an unsafe report would make it safe, the report instead counts as safe.
;;
;; More of the above example's reports are now safe:
;;
;;     7 6 4 2 1: Safe without removing any level.
;;     1 2 7 8 9: Unsafe regardless of which level is removed.
;;     9 7 6 2 1: Unsafe regardless of which level is removed.
;;     1 3 2 4 5: Safe by removing the second level, 3.
;;     8 6 4 4 1: Safe by removing the third level, 4.
;;     1 3 6 7 9: Safe without removing any level.
;;
;; Thanks to the Problem Dampener, 4 reports are actually safe!
;;
;; Update your analysis by handling situations where the Problem Dampener can remove a single level from unsafe reports. How many reports are now safe?

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  ;; so the obvious solution is to brute-force this:
  ;; 1. For each report, generate all variant reports; i.e. versions of the report with one level
  ;; removed. The size of the full set of variant reports will equal the length of the original
  ;; report. E.g. [1 2 3] will generate the set of variant reports [[1 2], [1 3], [2 3]].
  ;; 2. Analyze each variant report for safety. If any of the variant reports are safe, mark the
  ;; original report as safe.
  ;; 3. Count the number of safe reports the same as before.
  ;; As an obvious optimization, only analyze the set of variant reports if the original report comes
  ;; back as unsafe. Given that we know from pt. 1 that 37% of the reports are safe, this optimization
  ;; will be significant.
  ;;
  ;; looking at the input file, this seems like it will probably still be reasonably fast (under one
  ;; minute execution time).
  ;; But the question is: Is there a way of analyzing the reports such that their safety can be
  ;; determined without having to brute force?
  ;; An optional problem for later, I guess!

  ;; ok so to start, here's our soln function from pt. 1:
  (defn solve-pt1 [input]
    (->> input
         (map
          (fn [report]
            (and (or (apply > report) (apply < report))
                 (->> report
                      (partition 2 1)
                      (map #(abs (reduce - %)))
                      (map #(and (>= % 1) (<= % 3)))
                      (reduce #(and %1 %2))))))
         (filter true?)
         count))
  (solve-pt1 test-input) ;; => 2

  ;; we want to modify this so that if a report comes back as unsafe, we analyze the variant reports.
  ;; let me write a function that analyzes the variant reports for a given input report.
  ;; i'll start by defining a test report from one of the examples that's safe by dampening:
  (def test-report (nth test-input 3))
  ;; => (1 3 2 4 5)

  ;; now lemme generate all the variant reports for this report.
  ;; based on research it seems like keep-indexed will be reasonably concise:
  (map-indexed
   (fn [idx val]
     (keep #(when)))
   test-report)
  ;; i mean that seems like it will work, but it's kind of gross
  ;; split-at also seems promising:
  (split-at 2 test-report)
  (split-at 0 test-report)
  (let [[a b] (split-at 0 test-report)]
    (concat a (rest b))) ;; => (3 2 4 5)
  (map
   (fn [i]
     (let [[a b] (split-at i test-report)]
       (concat a (rest b))))
   (range (count test-report)))
  ;; => ((3 2 4 5) (1 2 4 5) (1 3 4 5) (1 3 2 5) (1 3 2 4))
  ;; so this works. it's a little weird that we're mapping over (range (count test-report)) instead
  ;; of the test report itself (the map is a closure over the test-report), but it's pretty elegant, i think.
  ;; let's wrap this in a function:
  (defn generate-variants [report]
    (map
     (fn [i]
       (let [[a b] (split-at i report)]
         (concat a (rest b))))
     (range (count report))))

  ;; ok so now we want to analyze the original report, and if false, analyze the variant reports
  ;; let's take the solution from pt 1 for analyzing a single report and put that in its own function:
  (defn report-safe? [report]
    (and (or (apply > report) (apply < report))
         (->> report
              (partition 2 1)
              (map #(abs (reduce - %)))
              (map #(and (>= % 1) (<= % 3)))
              (reduce #(and %1 %2)))))
  ;; and then we can try to wire it all together:
  (defn solve-pt2 [input]
    (->> input
         (map
          (fn [report]
            (or (report-safe? report)
                (some report-safe? (generate-variants report)))))
         (filter true?)
         count))
  ;; i just want to verify that (or nil) is false:
  (or false false) ;; => false
  (or nil nil) ;; => nil
  (or nil false) ;; => false
  ;; ok so that will be ok bc the first predicate is guaranteed to return a boolean
  ;; so let's test our solution:
  (solve-pt2 test-input)
  ;; => 4
  ;; ok that checks! lemme try to submit:
  (solve-pt2 input)
  ;; => 439
  ;; accepted!
  ())
