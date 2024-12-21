(ns advent-of-code-2024.09.journal
  (:require [clojure.string :as str]))

;; PART ONE
;;
;; Another push of the button leaves you in the familiar hallways of some friendly amphipods! Good thing you each somehow got your own personal mini submarine. The Historians jet away in search of the Chief, mostly by driving directly into walls.
;;
;; While The Historians quickly figure out how to pilot these things, you notice an amphipod in the corner struggling with his computer. He's trying to make more contiguous free space by compacting all of the files, but his program isn't working; you offer to help.
;;
;; He shows you the disk map (your puzzle input) he's already generated. For example:
;;
;; 2333133121414131402
;;
;; The disk map uses a dense format to represent the layout of files and free space on the disk. The digits alternate between indicating the length of a file and the length of free space.
;;
;; So, a disk map like 12345 would represent a one-block file, two blocks of free space, a three-block file, four blocks of free space, and then a five-block file. A disk map like 90909 would represent three nine-block files in a row (with no free space between them).
;;
;; Each file on disk also has an ID number based on the order of the files as they appear before they are rearranged, starting with ID 0. So, the disk map 12345 has three files: a one-block file with ID 0, a three-block file with ID 1, and a five-block file with ID 2. Using one character for each block where digits are the file ID and . is free space, the disk map 12345 represents these individual blocks:
;;
;; 0..111....22222
;;
;; The first example above, 2333133121414131402, represents these individual blocks:
;;
;; 00...111...2...333.44.5555.6666.777.888899
;;
;; The amphipod would like to move file blocks one at a time from the end of the disk to the leftmost free space block (until there are no gaps remaining between file blocks). For the disk map 12345, the process looks like this:
;;
;; 0..111....22222
;; 02.111....2222.
;; 022111....222..
;; 0221112...22...
;; 02211122..2....
;; 022111222......
;;
;; The first example requires a few more steps:
;;
;; 00...111...2...333.44.5555.6666.777.888899
;; 009..111...2...333.44.5555.6666.777.88889.
;; 0099.111...2...333.44.5555.6666.777.8888..
;; 00998111...2...333.44.5555.6666.777.888...
;; 009981118..2...333.44.5555.6666.777.88....
;; 0099811188.2...333.44.5555.6666.777.8.....
;; 009981118882...333.44.5555.6666.777.......
;; 0099811188827..333.44.5555.6666.77........
;; 00998111888277.333.44.5555.6666.7.........
;; 009981118882777333.44.5555.6666...........
;; 009981118882777333644.5555.666............
;; 00998111888277733364465555.66.............
;; 0099811188827773336446555566..............
;;
;; The final step of this file-compacting process is to update the filesystem checksum. To calculate the checksum, add up the result of multiplying each of these blocks' position with the file ID number it contains. The leftmost block is in position 0. If a block contains free space, skip it instead.
;;
;; Continuing the first example, the first few blocks' position multiplied by its file ID number are 0 * 0 = 0, 1 * 0 = 0, 2 * 9 = 18, 3 * 9 = 27, 4 * 8 = 32, and so on. In this example, the checksum is the sum of these, 1928.
;;
;; Compact the amphipod's hard drive using the process he requested. What is the resulting filesystem checksum? (Be careful copy/pasting the input for this puzzle); it is a single, very long line.)

(def test-input-raw "2333133121414131402")

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  ;; seems pretty clear that there's some sort of math shortcut here, esp. if you were using
  ;; an imperative style, but i think it would be better practice for my purposes if i just
  ;; do the obvious thing and simulate the problem scenario fully.

  ;; parse input
  (defn parse-input [raw-input]
    (let [nums (map #(Integer/parseInt (str %)) raw-input)]
      ;; for now i'm going to make the simplifying assumption that the last
      ;; value in every input always represents a block; i.e. that the length
      ;; of the input is always odd and thus the number of files will always
      ;; be one more than the number of free spaces.
      (->> (rest nums)
           (take-nth 2)
           (into [])
           ;; TODO: this could be cleaned up if there were a way to change
           ;; the behavior of map such that it padded the shorter of two
           ;; input collections with nil
           (#(conj % 0))
           (mapv vector (take-nth 2 nums))
           (reduce-kv
            (fn [acc idx [disk-size free-space-size]]
              (concat acc (repeat disk-size idx) (repeat free-space-size nil)))
            [])
           (into []))))

  (def test-input (parse-input test-input-raw))
  ;; => [0 0 nil nil nil 1 1 1 nil nil nil 2 nil nil nil 3 3 3 nil 4 4 nil 5 ... ]

  ;; compact disk (unfinished)
  (assoc [1 2 3], 0 5, 2 6)
  (let [v [1 2 3]]
    (assoc v, 0 (v 2), 2 (v 0)))
  (some #{0} test-input)
  (some some? [1 2 3 4])
  (some some? [nil nil nil])
  (some some? [nil nil 1 nil])
  (drop-while nil? [1 2 3 4])
  (drop-while nil? [nil nil 3 4])
  (let [input test-input]
    (loop [input input, i 0, acc 0]
      (if (and (not-empty input) (some some? input))
        (recur
         (->> input
              (drop-while nil?)
              (into [])
              (iterate (fn [v] (if #(nil? (peek v)) (pop v) v)))))
        acc)))

  ;; unfinished
  (def test-input (parse-input test-input-raw))
  (let [input test-input]
    (let [swap-blocks (fn [v i j] (assoc v, i (v j), j (v i)))]
      ;; i always points to the first free block
      ;; j always points to the last file block
      (loop [i 0, j (dec (count input)), input input]
        (if (< i j)
          (let [input (swap-blocks input i j)]
            (recur
             (find-next i input)
             (find-next j input)
             input))
          input))))

  ;; full soln (unfinished)
  (let [input test-input]
    (->> input
         (compact-disk)
         (take-while some?)
         (vec)
         (reduce-kv #(+ %1 (* %2 %3)) 0)))

  ;; okay i've been struggling with this for a bit and i'm realizing that a java array would actually
  ;; be best here, bc all of the clojure sequence methods return a list and i really do need a vector/array
  ;; in this case. And I need it to stay in that form the whole time.
  ;;
  ;; other takeaways so far:
  ;;   - working with an array and its indexes at the same time is hard
  ;;
  ;; okay what i did was i wrote the solution in python just to check that my solution isn't off
  ;; (it does not appear to be), so now i can just focus on translating that correct solution into
  ;; clojure idioms
  (char 48) ;; => \0
  (char 57) ;; => \9
  (char (+ 7 48)) ;; => \7
  (defn parse-input [raw-input]
    (let [nums (map #(Integer/parseInt (str %)) raw-input)]
      ;; for now i'm going to make the simplifying assumption that the last
      ;; value in every input always represents a block; i.e. that the length
      ;; of the input is always odd and thus the number of files will always
      ;; be one more than the number of free spaces.
      (->> (rest nums)
           (take-nth 2)
           (into [])
           ;; TODO: this could be cleaned up if there were a way to change
           ;; the behavior of map such that it padded the shorter of two
           ;; input collections with nil
           (#(conj % 0))
           (mapv vector (take-nth 2 nums))
           (reduce-kv
            (fn [acc idx [disk-size free-space-size]]
              (concat acc
                      ;; add 48 to get ascii code for corresponding numeral character
                      (repeat disk-size (char (+ idx 48)))
                      (repeat free-space-size \.)))
            [])
           (into []))))

  (def test-input (parse-input test-input-raw))

  (defn next-free [disk i]
    (loop [idx i]
      (if (and (< idx (count disk)) (not= nil (disk idx)))
        (recur (inc idx))
        (if (= idx (count disk)) nil idx))))

  (defn prev-file [disk j]
    (loop [idx j]
      (if (and (>= idx 0) (= nil (disk idx)))
        (recur (dec idx))
        (if (< idx 0) nil idx))))
  (count test-input)
  (prev-file test-input 35)

  (defn compact-disk [disk]
    (let [swap-blocks (fn [v i j] (assoc v, i (v j), j (v i)))]
      (loop [i (next-free disk 0)
             j (prev-file disk (dec (count disk)))
             disk disk]
        (if (< i j)
          (let [disk (swap-blocks disk i j)]
            (recur
             (next-free disk i)
             (prev-file disk j)
             disk))
          disk))))
  (compact-disk test-input)
  ;; => [\0 \0 \9 \9 \8 \1 \1 \1 \8 \8 \8 \2 \7 \7 \7 \3 \3 \3 \6 \4 \4
  ;;     \6 \5 \5 \5 \5 \6 \6 \. \. \. \. \. \. \. \. \. \. \. \. \. \.]

  (let [input test-input]
    (->> input
         (compact-disk)
         (take-while #(not= \. %))
         (mapv #(Integer/parseInt (str %)))
         (reduce-kv (fn [acc idx file-id] (+ acc (* idx file-id))) 0)))
  ;; 1928
  ;; okay, finally got it.
  ;; let's try on the real input:
  (defn solve-pt1 [input]
    (->> input
         (compact-disk)
         (take-while some?)
         #_(mapv #(Integer/parseInt (str %)))
         (vec)
         (reduce-kv (fn [acc idx file-id] (+ acc (* idx file-id))) 0)))

  (def input (-> (slurp "src/advent_of_code_2024/09/input.txt")
                 (str/trim)
                 (parse-input)))
  (solve-pt1 input)

  ;; lol i get a stackoverflow trying to parse the input
  (let [v [1 2 3 4]]
    (->> (take-nth 2 v)
         (#(concat % [0]))))
  (defn parse-input [raw-input]
    (let [nums (map #(Integer/parseInt (str %)) (str/trim raw-input))
          file-sizes (take-nth 2 nums)
          free-sizes (concat (take-nth 2 (rest nums)) [0])]
      (->> (map vector file-sizes free-sizes)
           (map-indexed
            (fn [idx [file-size free-size]]
              (concat (repeat file-size idx)
                      (repeat free-size nil))))
           (flatten)
           (vec))))
  ;; okay this is more readable and doesn't appear to overflow on the real input
  (def input (parse-input (slurp "src/advent_of_code_2024/09/input.txt")))
  (solve-pt1 input)
  ;; okay i'm quite tired at this point and i forgot that the test input only has 9
  ;; files for illustrative purposes. the real input has many more than that, so if
  ;; i map all of those ids to characters, i get a lot of non-numeric characters.
  ;; so i actually do have to use a vec of numbers
  ;;
  ;; i fixed it in-place (no new code)
  (def input (parse-input (slurp "src/advent_of_code_2024/09/input.txt")))
  (solve-pt1 input)
  ;; => 6398608069280
  ;; accepted
  (loop [bindings]
    (if end-condition
      ret-value
      (recur new-bindings)))
  ())

(defn parse-input [raw-input]
  (let [nums (map #(Integer/parseInt (str %)) (str/trim raw-input))
        file-sizes (take-nth 2 nums)
        free-sizes (concat (take-nth 2 (rest nums)) [0])]
    (->> (map vector file-sizes free-sizes)
         (map-indexed
          (fn [idx [file-size free-size]]
            (concat (repeat file-size idx)
                    (repeat free-size nil))))
         (flatten)
         (vec))))

(def test-input (parse-input test-input-raw))

;; ====================================================================================
;; PART TWO
;;
;; Upon completion, two things immediately become clear. First, the disk definitely has a lot more contiguous free space, just like the amphipod hoped. Second, the computer is running much more slowly! Maybe introducing all of that file system fragmentation was a bad idea?
;;
;; The eager amphipod already has a new plan: rather than move individual blocks, he'd like to try compacting the files on his disk by moving whole files instead.
;;
;; This time, attempt to move whole files to the leftmost span of free space blocks that could fit the file. Attempt to move each file exactly once in order of decreasing file ID number starting with the file with the highest file ID number. If there is no span of free space to the left of a file that is large enough to fit the file, the file does not move.
;;
;; The first example from above now proceeds differently:
;;
;; 00...111...2...333.44.5555.6666.777.888899
;; 0099.111...2...333.44.5555.6666.777.8888..
;; 0099.1117772...333.44.5555.6666.....8888..
;; 0099.111777244.333....5555.6666.....8888..
;; 00992111777.44.333....5555.6666.....8888..
;;
;; The process of updating the filesystem checksum is the same; now, this example's checksum would be 2858.
;;
;; Start over, now compacting the amphipod's hard drive using this new method instead. What is the resulting filesystem checksum?

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  ;; defn swap-file

  ;; prev-file
  ([1 2 3] 7)
  (defn prev-file [input idx]
    (let [cur-id (if (< idx (count input)) (input idx) nil)]
      (loop [idx (dec idx)]
        (if (and (>= idx 0) (not= (input idx) nil) (not= (input idx) cur-id))
          (if (< idx 0) nil idx)
          (recur (dec idx))))))

  ;; cur file size
  (defn cur-file-size [disk idx]
    (let [file-id (disk idx)]
      (loop [i idx]
        (if (or (< i 0) (not= (disk i) file-id))
          (if (< i 0) (inc idx) (- idx i))
          (recur (dec i))))))

  ;; free space size
  (defn free-space-size [disk idx]
    (let [size (count disk)]
      (loop [i idx]
        (if (or (>= i size) (some? (disk i)))
          (if (>= i size) (- size idx) (- i idx))
          (recur (inc i))))))
  (free-space-size test-input 2)

  ;; find free space
  ;; TODO setting file-size to 4 breaks my repl for some reason
  (let [disk test-input, file-size 4]
    (loop [idx (next-free disk 0)]
      (if (and (< idx (count disk)) (>= (free-space-size disk idx) file-size))
        idx
        (if (>= idx (count disk)) nil
            (recur (next-free disk idx))))))
  (+ 4 5)

  ;; compact disk by file
  (let [disk test-input]
    (loop [i (next-free disk 0)
           ;; starting at (count disk) is not a typo
           j (prev-file disk (count disk))
           disk disk]
      (let [cur-id (disk j)
            file-size (cur-file-size disk j)])))
  ())
