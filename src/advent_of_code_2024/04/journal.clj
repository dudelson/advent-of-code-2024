(ns advent-of-code-2024.04.journal
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [clojure.set :as set]
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

(def test-input (parse-input test-input-raw))

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

(def stringify-matrix (partial map #(apply str %)))

;; ===============================================================
;; PART TWO
;; Looking for the instructions, you flip over the word search to find that this isn't actually an XMAS puzzle; it's an X-MAS puzzle in which you're supposed to find two MAS in the shape of an X. One way to achieve that is like this:
;;
;; M.S
;; .A.
;; M.S
;;
;; Irrelevant characters have again been replaced with . in the above diagram. Within the X, each MAS can be written forwards or backwards.
;;
;; Here's the same example from before, but this time all of the X-MASes have been kept instead:
;;
;; .M.S......
;; ..A..MSMS.
;; .M.S.MAA..
;; ..A.ASMSM.
;; .M.S.M....
;; ..........
;; S.S.S.S.S.
;; .A.A.A.A..
;; M.M.M.M.M.
;; ..........
;;
;; In this example, an X-MAS appears 9 times.
;;
;; Flip the word search from the instructions back over to the word search side and try again. How many times does an X-MAS appear?

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  ;; okay so the way to solve this i think is to look for "MAS" or "SAM" on the diagonals only,
  ;; and to note the coords of the central letter (which is always "A"). I'll have a list of
  ;; coords from the major diagonals, and a list of coords from the minor diagonals. Any time
  ;; a major coord and a minor coord match, that's a full match.
  ;;
  ;; the part i'm not sure about is whether the major and minor coord systems are the same,
  ;; or if there's some translation that has to be done
  ;; To test this, i'm going to insert a special character into the test input and see where
  ;; it appears in each diagonal representation.
  (def test-input-raw "
MMM.XXMASM
M.AMXMSMSA
AMXSXMAAMM
MSAMASMS.X
XM@SAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
    ")

  (def test-input (parse-input test-input-raw))
  (def test-matrix (matrix/array (map vec test-input)))
  (major-diagonals test-matrix)
  (minor-diagonals test-matrix)
  ;; okay it's in completely different places, i think i have to work out the math for this
  ;;
  ;; Major Diagonals:
  ;;   - the offset from the center row (longest row) is equal to the difference between the
  ;;     x and y coords
  ;;   - if x-y is negative it's below, positive it's above
  ;;   - the x and y coords each increment by 1 as you go along each diagonal, so the sum of
  ;;     x and y increases by 2
  ;;   - so the column index (index w/in the row) of the val is going to be equal to the y
  ;;     coord for rows above the center, and the x coord for rows below the center
  ;;   - and then to get the other coord, you just add the row index (where the center is 0,
  ;;     rows below center are negative, and rows above center are positive)
  ;;   - i'm continuing to assume that the crossword is square, so there's going to be
  ;;     size-1 rows above center, and size-1 rows below center
  (case (math/signum 0)
    0.0 "zero"
    -1.0 "neg"
    1.0 "pos")
  (let [r -2 w 6]
    (let [c1 w c2 (+ w (abs r))]
      (case (math/signum r)
        0.0 [c1 c2]
        -1.0 [c2 c1]
        1.0 [c1 c2])))
  ;; [ 0  0] => [0 0]
  ;; [-3  0] => [3 0]
  ;; [-5  3] => [8 3]
  ;; [ 2  2] => [2 4]
  ;; these are all correct
  (defn major-diagonal->coords [r w]
    ;; r is between -(size-1) and size-1 inclusive
    ;; w is between 0 and size-1 inclusive
    (let [c1 w c2 (+ w (abs r))]
      (case (math/signum r)
        0.0 [c1 c2]
        -1.0 [c2 c1]
        1.0 [c1 c2])))

  ;; okay that's the major diagonals taken care of
  ;; now for the minor diagonals:
  ;;   - so we do a similar translation, except [0 0] in the diagonal repr is equal to
  ;;     [0 (size-1)] in the standard repr. So y is now `(- size ColIdx 1)`.
  ;;   - the property of the main minor diagonal is that the x y coords sum to size-1
  ;;   - and then as you go out from that, it steps down on either side.
  ;;     So x+y = size - RowIdx - 1
  ;; so actually from trial and error, it looks like i don't have to derive a formula
  ;; from scratch like i did for the major diagonals: in practice, the minor diagonal
  ;; conversion can just use the major diagonal conversion and then mirror the y coord
  ;; specifically
  (defn minor-diagonal->coords [r w size]
    (let [[x y] (major-diagonal->coords r w)]
      [x (- size y 1)]))
  (minor-diagonal->coords 3 2 10)
  ;; [ 7  1] => [1 1]  correct!
  ;; [ 6  3] => [3 0]  correct!
  ;; [-2  6] => [8 3]  correct!
  ;; [ 3  2] => [2 4]  correct!
  ;; okay great, that all seems to work

  ;; now i have to restore the test input
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
  (def test-input (parse-input test-input-raw))
  (def test-matrix (matrix/array (map vec test-input)))
  ;; so then for each diagonal we're going to look for "MAS" and "SAM" and note the position
  ;; of the middle letter
  (def test-string
    (-> test-matrix
        major-diagonals
        stringify-matrix
        (nth 9)))
  ;; => "MSXMAXSAMX"
  ;; so i have to get the index of each match within this string
  ;; research seems to indicate that i have to use java interop for this
  ;; so here is how i'd do one match
  (let [s test-string]
    (let [matcher (re-matcher #"SAM" s)]
      (when (re-find matcher)
        (. matcher start))))
  ;; => 6
  ;; and here's how i'd do all matches:
  (let [s test-string]
    (let [matcher (re-matcher #"X" s)]
      (for [idx (. matcher start)
            :while (re-find matcher)]
        idx)))
  ;; this doesn't work bc you can't call matcher.start() before calling matcher.find()
  ;; i don't think this was on track to be idiomatic in any case
  ;; more research indicates that a more elegant/idiomatic solution could be via using
  ;; `repeatedly`
  (let [s test-string]
    (let [matcher (re-matcher #"X" s)]
      (take-while some?
                  (repeatedly
                   #(when (re-find matcher) (. matcher start))))))
  ;; => (2 5 9)
  ;; beautiful

  (defn re-indexes [re s]
    (let [matcher (re-matcher re s)]
      (take-while some?
                  (repeatedly
                   #(when (re-find matcher) (. matcher start))))))

  (let [input test-input]
    (->> input
         (map vec)
         matrix/array
         major-diagonals
         stringify-matrix
         (map #(concat
                (re-indexes #"MAS" %)
                (re-indexes #"SAM" %)))))
  ;; this gives a sparse array with some numbers in it, with the shape matching that
  ;; of the diagonal repr, which is what i would expect

  (let [str-mtx (stringify-matrix (major-diagonals test-matrix))]
    ;; continuing to assume the word search is square
    (let [size (/ (+ (count str-mtx) 1) 2)]
      (->> str-mtx
           (map #(concat
                  (re-indexes #"MAS" %)
                  (re-indexes #"SAM" %)))
           (map-indexed
            (fn [idx data]
              (let [r (- size idx 1)]
              ;; add 1 bc we want the index of the 'A', not the beginning of the match
                (map #(major-diagonal->coords r (+ % 1)) data))))
        ;; can't use flatten bc we want to keep the inner-most level of nesting
           (reduce #(concat %1 %2) []))))
  ;; => ([1 8] [1 7] [3 7] [2 4] [5 7] [2 3] [5 6] [7 7] [2 1] [4 3] [6 2] [7 2])
  ;; appears to be working great
  (defn diagonal-matches [str-mtx conversion-fn]
    ;; continuing to assume the word search is square
    (let [size (/ (+ (count str-mtx) 1) 2)]
      (->> str-mtx
           (map #(concat
                  (re-indexes #"MAS" %)
                  (re-indexes #"SAM" %)))
           (map-indexed
            (fn [idx data]
              (let [r (- size idx 1)]
              ;; add 1 bc we want the index of the 'A', not the beginning of the match
                (map #(conversion-fn r (+ % 1)) data))))
        ;; can't use flatten bc we want to keep the inner-most level of nesting
           (reduce #(concat %1 %2) [])
           set)))
  (diagonal-matches (stringify-matrix (major-diagonals test-matrix)) major-diagonal->coords)
  (let [op *]
    (op 3 2))
  (merge #{} #{1 2} #{2 3})

  (let [input test-input
        input-matrix (matrix/array (map vec input))
        size (first (matrix/shape input-matrix))
        pipeline-fn #(-> input-matrix
                         %1
                         stringify-matrix
                         (diagonal-matches %2))]
    (count
     (set/intersection
      (pipeline-fn major-diagonals major-diagonal->coords)
      (pipeline-fn minor-diagonals #(minor-diagonal->coords %1 %2 size)))))
  ;; => 9
  ;; i have officially gotten the correct answer for the test input
  ;; now I will run on the actual input
  (defn solve-pt2 [input]
    (let [input-matrix (matrix/array (map vec input))
          size (first (matrix/shape input-matrix))
          pipeline-fn #(-> input-matrix
                           %1
                           stringify-matrix
                           (diagonal-matches %2))]
      (count
       (set/intersection
        (pipeline-fn major-diagonals major-diagonal->coords)
        (pipeline-fn minor-diagonals #(minor-diagonal->coords %1 %2 size))))))
  (solve-pt2 input)
  ;; => 1969
  ;; accepted (!)
  ())
