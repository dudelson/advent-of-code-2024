;; Design journal for Advent of Code 2024, Day 1
;; https://adventofcode.com/2024/day/1
(ns advent-of-code-2024.01
  (:require [clojure.string :as str]))

;; test to make sure the repl is working!
(defn add-three [n]
  (+ 3 n))

(add-three 11) ;; => 14

;;   Problem Statement
;; ---------------------
;; Upon pouring into the office, everyone confirms that the Chief Historian is indeed nowhere to be found. Instead, the Elves discover an assortment of notes and lists of historically significant locations! This seems to be the planning the Chief Historian was doing before he left. Perhaps these notes can be used to determine which locations to search?
;;
;; Throughout the Chief's office, the historically significant locations are listed not by name but by a unique number called the location ID. To make sure they don't miss anything, The Historians split into two groups, each searching the office and trying to create their own complete list of location IDs.
;;
;; There's just one problem: by holding the two lists up side by side (your puzzle input), it quickly becomes clear that the lists aren't very similar. Maybe you can help The Historians reconcile their lists?
;;
;; For example:
;;
;; 3   4
;; 4   3
;; 2   5
;; 1   3
;; 3   9
;; 3   3
;;
;; Maybe the lists are only off by a small amount! To find out, pair up the numbers and measure how far apart they are. Pair up the smallest number in the left list with the smallest number in the right list, then the second-smallest left number with the second-smallest right number, and so on.
;;
;; Within each pair, figure out how far apart the two numbers are; you'll need to add up all of those distances. For example, if you pair up a 3 from the left list with a 7 from the right list, the distance apart is 4; if you pair up a 9 with a 3, the distance apart is 6.
;;
;; In the example list above, the pairs and distances would be as follows:
;;
;;     The smallest number in the left list is 1, and the smallest number in the right list is 3. The distance between them is 2.
;;     The second-smallest number in the left list is 2, and the second-smallest number in the right list is another 3. The distance between them is 1.
;;     The third-smallest number in both lists is 3, so the distance between them is 0.
;;     The next numbers to pair up are 3 and 4, a distance of 1.
;;     The fifth-smallest numbers in each list are 3 and 5, a distance of 2.
;;     Finally, the largest number in the left list is 4, while the largest number in the right list is 9; these are a distance 5 apart.
;;
;; To find the total distance between the left list and the right list, add up the distances between all of the pairs you found. In the example above, this is 2 + 1 + 0 + 1 + 2 + 5, a total distance of 11!
;;
;; Your actual left and right lists contain many location IDs. What is the total distance between your lists?

;;   Design Journal
;; ------------------
;; First of all, let's define the test data given in the problem description:
(def raw-test-data "
3   4
4   3
2   5
1   3
3   9
3   3
  ")
(-> raw-test-data
    str/trim)
;; => "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
(def raw-test-data' (-> raw-test-data
                        str/trim
                        str/split-lines))
;; => ["3   4" "4   3" "2   5" "1   3" "3   9" "3   3"]
(map (fn [el] (str/split el #"   ")) raw-test-data')
;; => (["3" "4"] ["4" "3"] ["2" "5"] ["1" "3"] ["3" "9"] ["3" "3"])
(apply map vector (map (fn [el] (str/split el #"   ")) raw-test-data'))
;; => (["3" "4" "2" "1" "3" "3"] ["4" "3" "5" "3" "9" "3"])
(map (fn [list] (map #(Integer/parseInt %) list))
     (apply map vector (map (fn [el] (str/split el #"   ")) raw-test-data')))
;; => ((3 4 2 1 3 3) (4 3 5 3 9 3))
;; so here finally i have parsed the raw input into the corresponding two lists of numbers.
;; but i'm curious whether i can clean this code up, potentially by a lot...
;; so we have "3   4" and we want to turn it into [3,4]
(map #(Integer/parseInt %) (str/split "3   4" #"   "))
(->> raw-test-data'
     (map
      (fn [s]
        (map #(Integer/parseInt %) (str/split s #"   "))))
     (apply map vector))
;; this version also works, but it's annoying that i have to store in an intermediate variable
;; halfway through in order to switch between the -> and ->> macros
;; upon research, there is a more flexible alternative, the as-> macro:
(def test-data'
  (as-> raw-test-data $
    (str/trim $)
    (str/split-lines $)
    (map
     (fn [s]
       (as-> s $$
         (str/split $$ #"   ")
         (map #(Integer/parseInt %) $$)))
     $)
    (apply map vector $)))
;; ~~it works~~
;; and actually i think because the trim and split-lines functions only take one arg, i can
;; rewrite everything to use the thread-last macro instead of the thread-as macro:
(def test-data'
  (->> raw-test-data
       str/trim
       str/split-lines
       (map
        (fn [s]
          (as-> s $
            (str/split $ #"   ")
            (map #(Integer/parseInt %) $))))
       (apply map vector)))
;; ~~IT WORKS~~. Final answer.
;; I'm just going to create a function for that and redefine the test data that way:
(defn parse-input-data [raw-test-data]
  (->> raw-test-data
       str/trim
       str/split-lines
       (map
        (fn [s]
          (as-> s $
            (str/split $ #"   ")
            (map #(Integer/parseInt %) $))))
       (apply map vector)))
(def test-data (parse-input-data raw-test-data))

;; Next, the actual data that we have to compute an answer for lives at this URL:
;; https://adventofcode.com/2024/day/1/input
;; So let's write a way to load that in as well.
;; A little bit of googling shows me that there doesn't appear to be a way of making http requests
;; built into clojure.core, so I can either pull in a 3rd-party dep or build the request using Java interop.
;; I'm going to try the latter first bc I only plan on making this one request; I will fall back to the
;; former if necessary.
(def http-client (java.net.http.HttpClient/newHttpClient))
(def http-request
  (-> (java.net.http.HttpRequest/newBuilder)
      (.uri (java.net.URI/create "https://httpbin.org/get"))
      (.build)))
(def http-response
  (.send http-client http-request (java.net.http.HttpResponse$BodyHandlers/ofString)))
(.body http-response)
;; great, that all works. Bundle it up in a function:
(defn http-request [url]
  (let [client (java.net.http.HttpClient/newHttpClient)
        request (-> (java.net.http.HttpRequest/newBuilder)
                    (.uri (java.net.URI/create url))
                    (.build))
        handler (java.net.http.HttpResponse$BodyHandlers/ofString)]
    (-> client
        (.send request handler)
        (.body))))
(def resp-body (http-request "https://httpbin.org/get"))
;; and now the important question is: Does this actually work for getting the data from the
;; advent of code website?
(def actual-data-raw (http-request "https://adventofcode.com/2024/day/1/input"))
;; apparently you can also use slurp for this (limited functionality and doesn't actually work in this case)
(def actual-data-raw (slurp "https://adventofcode.com/2024/day/1/input"))
;; "Puzzle inputs differ by user.  Please log in to get your puzzle input.\n"
;; lmao. So yes, but I need to authenticate. So not quite that simple...
;; Ok I poked around a little bit and authenticating using OAuth (the only thing Advent of Code supports)
;; seems very non-trivial, so I'm going to circumvent that for now.
;; I got the actual puzzle input from the site and it has been copy-pasted into a file
(def puzzle-input (parse-input-data (slurp "src/advent_of_code_2024/01.input.txt")))
(count (first puzzle-input))
;; => 1000
(take 5 (first puzzle-input))
;; => (17633 79440 44767 86871 66575)

;; OK now to actually solve the puzzle!!!!!!
;; we are just sorting the list and computing the difference between each successive member:
(defn puzzle-01 [input]
  (->> input
       (map sort)
       (apply map (fn [a b] (abs (- a b))))
       (reduce +)))
(puzzle-01 test-data)
;; => 11
;; that checks out. Now for the final test.....
(def puzzle-01-soln (puzzle-01 puzzle-input))
;; => 2756096
;; GOTTEM. One puzzle down.
