(ns advent-of-code-2024.04.journal
  (:require [clojure.string :as str]
            [clojure.core.matrix :as matrix]))

;; PART ONE
;;
;; As the search for the Chief continues, a small Elf who lives on the station tugs on your shirt; she'd like to know if you could help her with her word search (your puzzle input). She only has to find one word: XMAS.
;;
;; This word search allows words to be horizontal, vertical, diagonal, written backwards, or even overlapping other words. It's a little unusual, though, as you don't merely need to find one instance of XMAS - you need to find all of them. Here are a few ways XMAS might appear, where irrelevant characters have been replaced with .:
;;
;; ..X...
;; .SAMX.
;; .A..A.
;; XMAS.S
;; .X....
;;
;; The actual word search will be full of letters instead. For example:
;;
;; MMMSXXMASM
;; MSAMXMSMSA
;; AMXSXMAAMM
;; MSAMASMSMX
;; XMASAMXAMM
;; XXAMMXXAMA
;; SMSMSASXSS
;; SAXAMASAAA
;; MAMMMXMMMM
;; MXMXAXMASX
;;
;; In this word search, XMAS occurs a total of 18 times; here's the same word search again, but where letters not involved in any XMAS have been replaced with .:
;;
;; ....XXMAS.
;; .SAMXMS...
;; ...S..A...
;; ..A.A.MS.X
;; XMASAMX.MM
;; X.....XA.A
;; S.S.S.S.SS
;; .A.A.A.A.A
;; ..M.M.M.MM
;; .X.X.XMASX
;;
;; Take a look at the little Elf's word search. How many times does XMAS appear?

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  ;; so we want to generate the following transformations of this character matrix:
  ;;   1. as-is (no transformation)
  ;;   2. mirrored (y-axis)
  ;;   3. rotated 90deg
  ;;   4. rotated 90deg + mirrored
  ;;   5. N-diagonal
  ;;   6. N-diagonal + mirrored
  ;;   7. anti-N diagonal
  ;;   8. anti-N diagonal + mirrored
  ;; then for each one we just count the number of matches of "XMAS"
  ;; we don't have to worry about overlapping matches in the case of this particular search
  ;; string, bc all the characters in the search string are distinct from all others.
  ;;
  ;; Or actually i guess it would be more efficient to generate only 4 transformations
  ;; and search for both "XMAS" and "SAMX" in each one, which eliminates the mirrors.
  ;;
  ;; i could write transformation functions myself, but i've decided to bust out
  ;; core.matrix for this 😎
  (def test-input-raw "
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
    ")

  (defn parse-input [raw-input]
    (-> raw-input
        str/trim
        str/split-lines))

  (defn generate-transformations [matrix]
    ;; TODO generate lazy sequence of transformations
    [matrix])

  (def test-input (parse-input test-input-raw))

  ;; this is the shape of the final solution
  (->> test-input
       generate-transformations
       (map
        (fn [matrix]
           ;; takes in a transformation of the word search and returns the number
           ;; of times the search string is found in that transformation matrix
          (->> matrix
               first
               (re-seq #"XMAS")
               count)))
       (reduce +))
  ;; => 1
  ;; this is correct since we're only looking at the first row of the untransformed test input

  ;; now i'm going to pull in core.matrix to generate the transformations for real
  ;; first of all, i need to create a matrix of chars. it cannot be a full string for each
  ;; row bc then the transpositions won't be computed correctly.
  ;; maybe i can just do this?
  (vec "test")
  ;; oh yeah! it's that easy.
  ;; so therefore our matrix is:
  (def test-matrix (matrix/array (map vec test-input)))

  ;; and then the two functions i'm interested in are (transpose) and (diagonal)
  (matrix/transpose test-matrix)
  (matrix/diagonal test-matrix)
  (matrix/diagonal test-matrix -1)
  (matrix/diagonal test-matrix 9)
  (matrix/diagonal test-matrix -8)

  (matrix/rotate test-matrix 1 1)
  (matrix/transpose (matrix/rotate test-matrix 0 5))
  ;; so it's easy to get the first three transformations, but the final transformation,
  ;; what is apparently called the minor diagonals, doesn't have a corresponding function
  ;; in core.matrix AFAICT. So from my research i will have to manually reverse the order
  ;; of the rows (this seems like the simplest way, even if there's a chance it might
  ;; not be the most efficient)
  (str "------------------------------------------------")
  (vector 1 2)
  (matrix/diagonal
   (reduce
    #(apply matrix/swap-rows %1 %2)
    test-matrix
    (map #(vector % (- 9 %)) (range 5))))
  ;; that does it!
  ;; so i have to do the full diagonals impl now
  (map #(matrix/diagonal test-matrix %) (range -12 13))
  (remove empty? (map #(matrix/diagonal test-matrix %) (range -12 13)))
  (matrix/shape test-matrix)
  (apply + (matrix/shape test-matrix))
  ;; actually i'm going to make an educated guess that the real input is also a square
  (defn matrix->diagonals [input]
    (let [size (first (matrix/shape input))]
      (->> (range (- size) size)
           (map #(matrix/diagonal input %))
           (remove empty?))))

  (def major-diagonals matrix->diagonals)
  (defn minor-diagonals [input]
    (let [size (first (matrix/shape input))]
      (as-> input $
        (reduce
         #(apply matrix/swap-rows %1 %2)
         $
           ;; this math will work for both even and odd nums as long as it's floor division
         (map #(vector % (- size % 1)) (range (quot size 2))))
        (matrix->diagonals $))))

  (/ 3 2) ;; => 3/2
  (quot 3 2) ;; => 1
  (quot 5 2) ;; => 2

  ;; and then the last thing before we can put it all together is a function to
  ;; turn the matrix back into a vector of strings that we can search on:
  (first test-matrix) ;; => [\M \M \M \S \X \X \M \A \S \M]
  (str (first test-matrix)) ;; => "[\\M \\M \\M \\S \\X \\X \\M \\A \\S \\M]"
  ;; ok so that doesn't work
  (apply str (first test-matrix)) ;; => "MMMSXXMASM"
  ;; that's the ticket
  (def stringify-matrix (partial map #(apply str %)))
  (stringify-matrix test-matrix)

  ;; okay so now we can write our function to generate all the transformations
  ;; i want to put these things into a lazy sequence
  ;; ok after researching i still don't understand (lazy-seq) so i'm just going to do
  ;; a poor man's version even tho it is kind of gross
  (defn generate-transformation [mtx n]
    (case n
      1 (matrix/transpose mtx)
      2 (major-diagonals mtx)
      3 (minor-diagonals mtx)
      mtx))
  (str "generating transformations....")
  (generate-transformation test-matrix 0)
  (generate-transformation test-matrix 1)
  (generate-transformation test-matrix 2)
  (generate-transformation test-matrix 3)
  ;; ok these all work as expected!
  ;; now i will put it all together into the final solution for pt 1
  (let [input test-input
        input-matrix (matrix/array (map vec input))]
    (map
     (fn [transformation-idx]
       (->> transformation-idx
            (generate-transformation input-matrix)
            stringify-matrix
            (map #(+
                   (count (re-seq #"XMAS" %))
                   (count (re-seq #"SAMX" %))))))
     (range 4)))
  ;; => ((1 1 0 0 2 0 0 0 0 1) (0 0 0 0 0 0 1 0 0 2) (0 0 0 1 0 1 0 0 0 1 1 0 0 1 0 0 0 0 0) (0 0 0 0 0 1 0 0 0 0 1 0 2 0 1 0 0 0 0))
  (let [input test-input
        input-matrix (matrix/array (map vec input))]
    (->> (range 4)
         (map
          (fn [transformation-idx]
            (->> transformation-idx
                 (generate-transformation input-matrix)
                 stringify-matrix
                 (map #(vector
                        (count (re-seq #"XMAS" %))
                        (count (re-seq #"SAMX" %)))))))
         flatten
         (apply +)))
  ;; => 18
  ;; okay, that's the correct result for the test input
  ;; i'll make a function and try it on the real input:
  (defn solve-pt1 [input]
    (let [input-matrix (matrix/array (map vec input))]
      (->> (range 4)
           (map
            (fn [transformation-idx]
              (->> transformation-idx
                   (generate-transformation input-matrix)
                   stringify-matrix
                   (map #(vector
                          (count (re-seq #"XMAS" %))
                          (count (re-seq #"SAMX" %)))))))
           flatten
           (apply +))))

  (def input (parse-input (slurp "src/advent_of_code_2024/04/input.txt")))
  (solve-pt1 input)
  ;; => 2507
  ;; accepted! =D
  ;; that one was actually super fun. very elegant solution in clojure.
  ;; it's sick that they can just bolt on array manipulation as a library and have it feel
  ;; like a natural extension of the core language.
  ())
