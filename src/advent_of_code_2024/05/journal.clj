(ns advent-of-code-2024.05.journal
  (:require [clojure.string :as str]))

;; NOTE: The formatter that automatically runs on save is breaking my code for pt 2. I have
;; manually fixed it in the solution file, but many of the forms in part 2 of this file have been
;; autoformatted such that they're different from how I wrote them and do not work.

;; PART ONE
;;
;; The North Pole printing department is busier than ever this close to Christmas, and while The Historians continue their search of this historically significant facility, an Elf operating a very familiar printer beckons you over.
;;
;; The Elf must recognize you, because they waste no time explaining that the new sleigh launch safety manual updates won't print correctly. Failure to update the safety manuals would be dire indeed, so you offer your services.
;;
;; Safety protocols clearly indicate that new pages for the safety manuals must be printed in a very specific order. The notation X|Y means that if both page number X and page number Y are to be produced as part of an update, page number X must be printed at some point before page number Y.
;;
;; The Elf has for you both the page ordering rules and the pages to produce in each update (your puzzle input), but can't figure out whether each update has the pages in the right order.
;;
;; For example:
;;
;; 47|53
;; 97|13
;; 97|61
;; 97|47
;; 75|29
;; 61|13
;; 75|53
;; 29|13
;; 97|29
;; 53|29
;; 61|53
;; 97|53
;; 61|29
;; 47|13
;; 75|47
;; 97|75
;; 47|61
;; 75|61
;; 47|29
;; 75|13
;; 53|13
;;
;; 75,47,61,53,29
;; 97,61,53,29,13
;; 75,29,13
;; 75,97,47,61,53
;; 61,13,29
;; 97,13,75,29,47
;;
;; The first section specifies the page ordering rules, one per line. The first rule, 47|53, means that if an update includes both page number 47 and page number 53, then page number 47 must be printed at some point before page number 53. (47 doesn't necessarily need to be immediately before 53); other pages are allowed to be between them.)
;;
;; The second section specifies the page numbers of each update. Because most safety manuals are different, the pages needed in the updates are different too. The first update, 75,47,61,53,29, means that the update consists of page numbers 75, 47, 61, 53, and 29.
;;
;; To get the printers going as soon as possible, start by identifying which updates are already in the right order.
;;
;; In the above example, the first update (75,47,61,53,29) is in the right order:
;;
;;     75 is correctly first because there are rules that put each other page after it: 75|47, 75|61, 75|53, and 75|29.
;;     47 is correctly second because 75 must be before it (75|47) and every other page must be after it according to 47|61, 47|53, and 47|29.
;;     61 is correctly in the middle because 75 and 47 are before it (75|61 and 47|61) and 53 and 29 are after it (61|53 and 61|29).
;;     53 is correctly fourth because it is before page number 29 (53|29).
;;     29 is the only page left and so is correctly last.
;;
;; Because the first update does not include some page numbers, the ordering rules involving those missing page numbers are ignored.
;;
;; The second and third updates are also in the correct order according to the rules. Like the first update, they also do not include every page number, and so only some of the ordering rules apply - within each update, the ordering rules that involve missing page numbers are not used.
;;
;; The fourth update, 75,97,47,61,53, is not in the correct order: it would print 75 before 97, which violates the rule 97|75.
;;
;; The fifth update, 61,13,29, is also not in the correct order, since it breaks the rule 29|13.
;;
;; The last update, 97,13,75,29,47, is not in the correct order due to breaking several rules.
;;
;; For some reason, the Elves also need to know the middle page number of each update being printed. Because you are currently only printing the correctly-ordered updates, you will need to find the middle page number of each correctly-ordered update. In the above example, the correctly-ordered updates are:
;;
;; 75,47,61,53,29
;; 97,61,53,29,13
;; 75,29,13
;;
;; These have middle page numbers of 61, 53, and 29 respectively. Adding these page numbers together gives 143.
;;
;; Of course, you'll need to be careful: the actual list of page ordering rules is bigger and more complicated than the above example.
;;
;; Determine which updates are already in the correct order. What do you get if you add up the middle page number from those correctly-ordered updates?

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  ;; the strategy here is to put the input rules into a map and then test
  ;; each item of each update vector against every single relevant rule.
  ;; There might be a more efficient way of verifying correctness, but
  ;; this is the safe way that's guaranteed to work AFAICT.

  ;; parse input format:
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

  ;; i can start with the input parser i used in problems 1 and 2, since
  ;; this input is fundamentally similar:
  (defn parse-input [raw-data]
    (->> raw-data
         str/trim
         str/split-lines
         (map
          (fn [s]
            (as-> s $
              (str/split $ #" ")
              (map #(Integer/parseInt %) $))))))

  ;; i believe when i run the first part on the test data, one of the lines
  ;; should be blank, which will be the input to (partition).
  (->> test-input-raw
       str/trim
       str/split-lines)

  (str/trim " efg ")

  ;; yeah so there's one blank line, so we do this to get the two parts of
  ;; the data:
  (->> test-input-raw
       str/trim
       str/split-lines
       (split-with (partial not= "")))
  ;; => [("47|53"
  ;;      "97|13"
  ;;      "97|61"
  ;;      "97|47"
  ;;      "75|29"
  ;;      "61|13"
  ;;      "75|53"
  ;;      "29|13"
  ;;      "97|29"
  ;;      "53|29"
  ;;      "61|53"
  ;;      "97|53"
  ;;      "61|29"
  ;;      "47|13"
  ;;      "75|47"
  ;;      "97|75"
  ;;      "47|61"
  ;;      "75|61"
  ;;      "47|29"
  ;;      "75|13"
  ;;      "53|13")
  ;;     (""
  ;;      "75,47,61,53,29"
  ;;      "97,61,53,29,13"
  ;;      "75,29,13"
  ;;      "75,97,47,61,53"
  ;;      "61,13,29"
  ;;      "97,13,75,29,47")]

  ;; i wanna know if we can destructure the result of str/split
  (str/split "75|13" #"|") ;; => ["7" "5" "|" "1" "3"]
  ;; oops
  (str/split "75|13" #"\|") ;; => ["75" "13"]
  (let [[l,r] (str/split "75|13" #"\|")]
    {:left l :right r})
  ;; => {:left "75", :right "13"}
  ;; nice

  ;; also can i supply a default value when using a key like a function to get a value from a map?
  (:foo {:foo 1 :bar 2} 0) ;; => 1
  (:foo {:baz 1 :bar 2} 0) ;; => 0
  ;; looks like yes i can
  ;; ok so you can use (update) to do this more concisely
  ;; but when the key doesn't exist, the update fn gets passed nil, so now i need to know
  ;; what happens when you conj or append onto nil
  ;; yes it does work
  (conj nil 2) ;; => (2)

  (let [[rules, updates]
        (->> test-input-raw
             str/trim
             str/split-lines
             (split-with (partial not= "")))]
    {:rules
     (reduce
      (fn [acc s]
        (let [rule (str/split s #"\|")
              [l,r] rule]
          (-> acc
              (update l #(conj % rule))
              (update r #(conj % rule)))))
      {}
      rules)
     ;; we throw away the first member of updates since that's the blank line that we split on
     :updates (map #(str/split % #",") (rest updates))})
  ;; => {:rules {"13" (["53", "13"], ...), "29": (...), ...}
  ;;     :updates (["75" "47" "61" "53" "29"], [...], ...)}

  ;; so that becomes the body of our (parse-input) function:
  (defn parse-input [raw-data]
    (let [[rules, updates]
          (->> test-input-raw
               str/trim
               str/split-lines
               (split-with (partial not= "")))]
      {:rules
       (reduce
        (fn [acc s]
          (let [rule (str/split s #"\|")
                [l,r] rule]
            (-> acc
                (update l #(conj % rule))
                (update r #(conj % rule)))))
        {}
        rules)
       :updates (map #(str/split % #",") (rest updates))}))

  (def test-input (parse-input test-input-raw))

  ;; so now we can take the map output by (parse-input) and use it to solve the puzzle:
  (nth [1 2 3] 1) ;; => 2

  ;; TODO:
  ;;  - move the code that makes the input into a map into its own function
  ;;  - refactor it so that rules are of the form [:before 45] or [:after 23]
  ;;     (i.e. relative to the value of the key)
  ;;  - test the filter here to make sure it works
  ;;     if that part works, then the rest will work bc its all trivial
  (defn solve-pt1 [input]
    (->> (:updates input)
         (filter
          (fn [update]
            (->> update
                 (map-indexed
                  (fn [idx val]
                    (let [rules (get-in input [:rules val])]
                      (every?
                       (fn [rule]
                         (let [dir (first rule)
                               other (second rule)
                               other-idx (.indexOf update other)]
                           ;; if `other` is not found, the rule is trivially satisfied
                           (if (= -1 other-idx)
                             true
                             ;; otherwise we have to compare indices (i.e. enforce the rule)
                             (case dir
                               :before (< idx other-idx)
                               :after  (> idx other-idx)))))
                       rules))))
                 (every? true?))))
      ;; extract the middle digit
         (map #(nth % (quot (count %) 2)))
         (reduce + 0)))

  ;; hello there it's a new day and we're back with more clojure goodness
  ;; i'm going to take care of that TODO above
  ;; here's the new parse function which only does the first part of the parse function i had before
  (defn parse-input [raw-data]
    (->> raw-data
         str/trim
         str/split-lines
         (split-with (partial not= ""))))

  ;; and then there's a new create-input-map function which does the second part, but uses keywords
  ;; instead of ordering:
  (defn create-input-map [[rules-raw updates-raw]]
    {:rules
     (reduce
      (fn [acc s]
        (let [[l,r] (str/split s #"\|")]
          (-> acc
              (update l #(conj % [:before r]))
              (update r #(conj % [:after l])))))
      {}
      rules-raw)
     :updates (map #(str/split % #",") (rest updates-raw))})

  (def test-input (create-input-map (parse-input test-input-raw)))
  ;; alright looks good

  ;; and then we can test our solution fn above (i will not copy it down here)
  (solve-pt1 test-input) ;; => 0
  ;; hah 0 is not correct so we have some debugging to do
  (filter
   (fn [update]
     (->> update
          (map-indexed
           (fn [idx val]
             (let [rules (get-in test-input [:rules val])]
               (every?
                (fn [rule]
                  (let [dir (first rule)
                        other (second rule)
                        other-idx (.indexOf update other)]
                           ;; if `other` is not found, the rule is trivially satisfied
                    (if (= -1 other-idx)
                      true
                             ;; otherwise we have to compare indices (i.e. enforce the rule)
                      (case dir
                        :before (< idx other-idx)
                        :after  (> idx other-idx)))))
                rules))))
          (every? true?)))
   (:updates test-input))
  ;; => ()
  ;; okay let's just take one value and compare it against all the rules
  (let [rules (get-in test-input [:rules "47"])
        update (first (:updates test-input))
        idx 1]
    (map ;; changed this from every? to map for eval purposes
     (fn [rule]
       (let [dir (first rule)
             other (second rule)
             other-idx (.indexOf update other)]
                          ;; if `other` is not found, the rule is trivially satisfied
         ;;(println (str "evaluating rule " rule " for value 47 on update " update))
         ;;(println (str "index of " other " is " other-idx))
         (if (= -1 other-idx)
           true
                            ;; otherwise we have to compare indices (i.e. enforce the rule)
           (case dir
             :before (< idx other-idx)
             :after  (> idx other-idx)))))
     rules))
  ;; => (false false false true true false)
  ;; this first update that we're testing on is in the right order according to the puzzle description,
  ;; so i would expect all rule checks to come back true.
  ;; i added some printlns to the above and will try again
  ;;
  ;; evaluating rule [:before "29"] for value 47 on update ["75" "47" "61" "53" "29"]
  ;; index of 29 is 4)
  ;; i(falseevaluating rule [:before "61"] for value 47 on update ["75" "47" "61" "53" "29"])
  ;; iindex of 61 is 2
  ;; i
  ;; i falseevaluating rule [:after "75"] for value 47 on update ["75" "47" "61" "53" "29"]
  ;; iindex of 75 is 0
  ;; i
  ;; i falseevaluating rule [:before "13"] for value 47 on update ["75" "47" "61" "53" "29"]
  ;; iindex of 13 is -1
  ;; i
  ;; i trueevaluating rule [:after "97"] for value 47 on update ["75" "47" "61" "53" "29"]
  ;; iindex of 97 is -1
  ;; i
  ;; i trueevaluating rule [:before "53"] for value 47 on update ["75" "47" "61" "53" "29"]
  ;; iindex of 53 is 3
  ;; i
  ;; i false
  ;;
  ;; okay it looks like i had the args switched around on those comparisons all the way on the very
  ;; inside of my function
  ;; now if i get rid of the printlns and eval i get (true true true true true true)
  ;;
  ;; now if i make the fix to the form before that and eval, i get:
  ;; => (["75" "47" "61" "53" "29"] ["97" "61" "53" "29" "13"] ["75" "29" "13"])
  ;; which is correct
  ;; then if i correct the full function and run, i get:
  (solve-pt1 test-input)
  ;; it errors bc i'm trying to > on a string?? the indexes should be numbers....
  ;; right ok they do need to be ints bc we need to add them at the end
  ;; i'll update create-input-map to do that:
  (defn create-input-map [[rules-raw updates-raw]]
    {:rules
     (reduce
      (fn [acc s]
        (let [[l,r] (str/split s #"\|")]
          (-> acc
              (update l #(conj % [:before r]))
              (update r #(conj % [:after l])))))
      {}
      rules-raw)
     :updates
     (map
      (fn [s] (map #(Integer/parseInt %) (str/split s #",")))
      (rest updates-raw))})
  (def test-input (create-input-map (parse-input test-input-raw)))
  (solve-pt1 test-input)
  ;; => 278
  ;; hm ok i was supposed to get 143...
  ;; i'm tired of scrolling up so i'm going to paste the soln function down here again
  (defn solve-pt1 [input]
    (->> (:updates input)
         (filter
          (fn [update]
            (->> update
                 (map-indexed
                  (fn [idx val]
                    (let [rules (get-in input [:rules val])]
                      (every?
                       (fn [rule]
                         (let [dir (first rule)
                               other (second rule)
                               other-idx (.indexOf update other)]
                           ;; if `other` is not found, the rule is trivially satisfied
                           (if (= -1 other-idx)
                             true
                             ;; otherwise we have to compare indices (i.e. enforce the rule)
                             (case dir
                               :before (< idx other-idx)
                               :after  (> idx other-idx)))))
                       rules))))
                 (every? true?))))
      ;; extract the middle digit
         (map #(nth % (quot (count %) 2)))))
  ;; so i just got rid of the final reduce and i want to see what numbers i get
  (solve-pt1 test-input)
  ;; => (61 53 29 47 13 75)
  ;; huh so i got *every* middle digit, which means the filter still isn't working

  ;; I actually really don't like my solution so I am going to go back to the drawing board
  ;; and think about this more carefully. I am thinking I can use the `for` expr (list
  ;; (comprehension) that i learned about last night after i wrote the initial soln above.
  ;; here are my notes:
  ;;
  ;; for each i in the update
  ;;   for each j after i
  ;;
  ;;
  ;; 97|13
  ;; 75, 97, 13
  ;; i = 97, j = 13
  ;; rules[97] = #[13]  ;; 97 must be before *all of these digits*
  ;;
  ;; so we're looking for:
  ;; 75|97, 75|13
  ;; 97|13
  ;;
  ;; 97|13 means that 97 *must* appear before 13 if both numbers are present in the sequence
  ;; is that the same as saying that 13 *must not* appear before 97?
  ;;
  ;; so for every pair (i,j) in our comprehension (where j appears after i), we want to know if
  ;; there's a rule in the ruleset that relates i and j. There are three possibilities:
  ;;
  ;;   1. There is no rule in the ruleset which relates i and j.
  ;;        In this case, the update has not been ruled out as valid, so we should continue processing.
  ;;   2. We find the rule "i|j".
  ;;        This matches the order the numbers appear in the update, so in this case, the update
  ;;        has not been ruled out as valid, so we should continue processing.
  ;;   3. We find the rule "j|i".
  ;;        This automatically rules out the update as valid, at which point we immediately
  ;;        return `false` for the update in question.
  ;;
  ;; If we get to the end of the comprehension, then we return `true` for the update in question.
  ;;
  ;; IOW, all we care about is that all the relevant rules are satisfied. The relevant rules "x|y"
  ;; are the ones for which both x and y appear in the update. Per the definition of our list
  ;; comprehension, we will iterate over all such pairs (x,y) that are relevant. And the only way
  ;; an update can be invalid is if it breaks a rule in the ruleset, since the way the rules are
  ;; phrased implies that an empty ruleset would make all updates valid.
  ;;
  ;; so i should map each number to the set of numbers that would invalidate the update
  ;; if they appeared afterwards, and then i can efficiently check for rule violations.

  ;; ok so first we have to tweak create-input-map to fit our new spec:
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

  (def test-input (create-input-map (parse-input test-input-raw)))

  ;; i'm proud of #(conj (or % #{}) l) as a clever and concise way of getting around the fact
  ;; that conj on nil will return a list (and i want a set).
  ;; i also removed the call to parseInt bc it makes :updates contain ints when :rules still
  ;; contains strings, which is probably why the prior soln was still broken. I'll just do
  ;; the conversion at the end before I add, which is more efficient anyway (bc i'm only
  ;; parsing the ints i actually need to treat as ints).

  ;; so now i will write a list comprehension that does what i describe above:
  (def test-update (first (:updates test-input)))
  (let [len (count test-update)]
    (for [i (range len)
          j (range len)
          :when (< i j)]
      [i j]))
  ;; => ([0 1] [0 2] [0 3] [0 4] [1 2] [1 3] [1 4] [2 3] [2 4] [3 4])
  ;; perfect.
  (defn validate [update]
    (let [len (count update)]
      (for [i (range len)
            j (range len)
            :when (< i j)]
        (let [ni (nth update i)
              nj (nth update j)]
          (not (contains? (get-in test-input [:rules ni]) nj))))))
  (validate test-update)
  ;; => (true true true true true true true true true true)
  ;; the first update is in the right order, so this is the correct result
  ;; and here is an update that should come back as invalid:
  (validate (nth (:updates test-input) 3))
  ;; => (false true true true true true true true true true)
  ;; this is correct but does a lot of extra work bc as you can see, the first result tells us
  ;; that the entire update is invalid. I think I can return early using a :while modifier:
  (defn validate [update]
    (let [len (count update)
          valid? (fn [i j] (not (contains? (get-in test-input [:rules (nth update i)]) (nth update j))))]
      (for [i (range len)
            j (range len)
            :when (< i j)
            :while (valid? i j)]
        [i j])))
  (validate test-update)
  (validate (nth (:updates test-input) 3))
  ;; ok so that doesn't work the way i want
  ;; i did a little bit of research, and this method is a little awkward anyway since i'm not
  ;; using the result of the list comprehension; i'm just using it as a means to an end.
  ;; i cannot find confirmation that (every?) returns early, but i can only assume that it does,
  ;; therefore i can use the for expr to only generate the indices (`for` is lazy) and move
  ;; the :while clause outside:
  (defn validate [update]
    (let [len (count update)]
      (every?
       (fn [[i j]]
         (not (contains?
               (get-in test-input [:rules (nth update i)])
               (nth update j))))
       (for [i (range len)
             j (range len)
             :when (< i j)]
         [i j]))))
  (validate test-update) ;; => true
  (validate (nth (:updates test-input) 3)) ;; => false
  ;; that works great.
  ;; so now i can do the full solution:
  (defn solve-pt1 [input]
    (->> (:updates input)
         (filter
          (fn [update]
            (let [len (count update)]
              (every?
               (fn [[i j]]
                 (not (contains?
                       (get-in input [:rules (nth update i)])
                       (nth update j))))
               (for [i (range len)
                     j (range len)
                     :when (< i j)]
                 [i j])))))))
  (solve-pt1 test-input)
  ;; => (["75" "47" "61" "53" "29"] ["97" "61" "53" "29" "13"] ["75" "29" "13"])
  ;; great. and now to do the rest:
  (defn solve-pt1 [input]
    (->> (:updates input)
         (filter
          (fn [update]
            (let [len (count update)]
              (every?
               (fn [[i j]]
                 (not (contains?
                       (get-in input [:rules (nth update i)])
                       (nth update j))))
               (for [i (range len)
                     j (range len)
                     :when (< i j)]
                 [i j])))))
         ;;(map #(Integer/parseInt (nth % (quot (count %) 2))))
         (map
          (fn [update]
            (-> update
                #(count %)
                #(nth update (quot % 2))
                #(Integer/parseInt %))))))
  ;; i'm getting a syntax error here for some reason. need to investigate:
  (-> test-update
      #(count %)
      #(nth test-update (quot % 2))
      #(Integer/parseInt %))
  (-> test-update
      count) ;; => 5
  (-> test-update
      count ;; => 5
      #(nth test-update 1)) ;; => error
  (-> test-update
      count ;; => 5
      #(+ 1 %)) ;; => error
  ;; ok so i'm pretty sure you can't pass functions to the threading macros bc that's not how they work
  (defn solve-pt1 [input]
    (->> (:updates input)
         (filter
          (fn [update]
            (let [len (count update)]
              (every?
               (fn [[i j]]
                 (not (contains?
                       (get-in input [:rules (nth update i)])
                       (nth update j))))
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
  (solve-pt1 test-input)
  ;; => 143
  ;; okay, now to try it on the real input
  (def input
    (-> "src/advent_of_code_2024/05/input.txt"
        slurp
        parse-input
        create-input-map))
  (solve-pt1 input)
  ;; => 5087
  ;; accepted
  ())

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

(def test-input (create-input-map (parse-input test-input-raw)))

(def input
  (-> "src/advent_of_code_2024/05/input.txt"
      slurp
      parse-input
      create-input-map))

;; ===================================================================================
;;   PART TWO
;;
;; While the Elves get to work printing the correctly-ordered updates, you have a little time to fix the rest of them.
;;
;; For each of the incorrectly-ordered updates, use the page ordering rules to put the page numbers in the right order. For the above example, here are the three incorrectly-ordered updates and their correct orderings:
;;
;;     75,97,47,61,53 becomes 97,75,47,61,53.
;;     61,13,29 becomes 61,29,13.
;;     97,13,75,29,47 becomes 97,75,47,29,13.
;;
;; After taking only the incorrectly-ordered updates and ordering them correctly, their middle page numbers are 47, 29, and 47. Adding these together produces 123.
;;
;; Find the updates which are not in the correct order. What do you get if you add up the middle page numbers after correctly ordering just those updates?

#_{:clj-kondo/ignore [:redefined-var]}
(comment
  (defn solve-pt2 [input]
    (->> input
        ;; this part is the same as pt1, except we change `filter` to `remove`
         (remove
          (fn [update]
            (let [len (count update)]
              (every?
               (fn [[i j]]
                 (not (contains?
                       (get-in test-input [:rules (nth update i)])
                       (nth update j))))
               (for [i (range len)
                     j (range len)
                     :when (< i j)]
                 [i j])))))))
  (solve-pt2 test-input) ;; => ()
  ;; this returns () when it should return 3 lists (the ones that are invalid)
  (def test-update (first (:updates test-input)))
  (let [len (count test-update)]
    (every?
     (fn [[ni nj]]
       (not (contains?
             (get-in test-input [:rules ni])
             nj)))
     (for [i (range len)
           j (range len)
           :when (< i j)]
       [(nth test-update i) (nth test-update j)])))
  ;; => true (which is correct)
  (def test-update (nth (:updates test-input) 3))
  (let [len (count test-update)]
    (every?
     (fn [[ni nj]]
       (not (contains?
             (get-in test-input [:rules ni])
             nj)))
     (for [i (range len)
           j (range len)
           :when (< i j)]
       [(nth test-update i) (nth test-update j)])))
  ;; true (which is incorrect)
  (let [len (count test-update)]
    (map
     (fn [[ni nj]]
       (not (contains?
             (get-in test-input [:rules ni])
             nj)))
     (for [i (range len)
           j (range len)
           :when (< i j)]
       [(nth test-update i) (nth test-update j)])))
  ;; => (true true true true true true true true true true)
  (let [len (count test-update)]
    (map
     (fn [[ni nj]]
       (get-in test-input [:rules ni]))
     (for [i (range len)
           j (range len)
           :when (< i j)]
       [(nth test-update i) (nth test-update j)])))
  ;; this output is long but it doesn't look right
  (:rules test-input)
  ;; this looks right
  ;; but there's no entry for 97 bc it doesn't appear second in any rules
  ;; i forgot to test that case:
  (contains? nil 7) ;; => false
  ;; ok it still works
  ;; so the confusing thing is just the rules being printed don't match what i would expect
  ;; i'm going to need more detail:
  (def test-update (nth (:updates test-input) 3))
  (let [len (count test-update)]
    (map
     (fn [[ni nj]]
       (let [rules (get-in test-input [:rules ni])
             result (not (contains? rules nj))]
         (str "[" ni "," nj "] rules for " ni ": " rules ", result: " result)))
     (for [i (range len)
           j (range len)
           :when (< i j)]
       [(nth test-update i) (nth test-update j)])))
  (let [len (count test-update)]
    (every?
     (fn [[ni nj]]
       (not (contains?
             (get-in test-input [:rules ni])
             nj)))
     (for [i (range len)
           j (range len)
           :when (< i j)]
       [(nth test-update i) (nth test-update j)])))
  ;; huh i think i was just using the wrong value of test-update the whole time?
  (defn solve-pt2 [input]
    (->> input
        ;; this part is the same as pt1, except we change `filter` to `remove`
         (remove
          (fn [update]
            (let [len (count update)]
              (every?
               (fn [[i j]]
                 (not (contains?
                       (get-in test-input [:rules (nth update i)])
                       (nth update j))))
               (for [i (range len)
                     j (range len)
                     :when (< i j)]
                 [i j])))))))
  (solve-pt2 test-input)
  ;; but this still returns empty list...
  (def test-update (nth (:updates test-input) 4))
  (def test-update (nth (:updates test-input) 5))
  (let [len (count test-update)]
    (every?
     (fn [[ni nj]]
       (not (contains?
             (get-in test-input [:rules ni])
             nj)))
     (for [i (range len)
           j (range len)
           :when (< i j)]
       [(nth test-update i) (nth test-update j)])))
  ;; but i'm getting the right values here...
  ;; maybe i just misunderstood how (remove) works?
  (remove odd? (range 10))
  (->> test-input
      ;; this part is the same as pt1, except we change `filter` to `remove`
       (map
        (fn [update]
          (let [len (count update)]
            (every?
             (fn [[i j]]
               (not (contains?
                     (get-in test-input [:rules (nth update i)])
                     (nth update j))))
             (for [i (range len)
                   j (range len)
                   :when (< i j)]
               [i j]))))))
  ;; BRO i was literally mapping over `input` instead of `(:updates input)`.......
  (defn solve-pt2 [input]
    (->> (:updates input)
        ;; this part is the same as pt1, except we change `filter` to `remove`
         (remove
          (fn [update]
            (let [len (count update)]
              (every?
               (fn [[ni nj]]
                 (not (contains?
                       (get-in test-input [:rules ni])
                       nj)))
               (for [i (range len)
                     j (range len)
                     :when (< i j)]
                 [(nth update i) (nth update j)])))))))
  (solve-pt2 test-input)
  ;; great now it works
  ;; so now i have to implement the meat of this part of the puzzle: if an index pair (i,j)
  ;; is found to be invalid, i have to swap the values at those indexes to make it valid
  ;; technically this could need to be done multiple time, but i'm going to try it w/o
  ;; rescans for now and see if i still get the right answer.

  ;; let's just do it with a single test update for now.
  ;; the key op is i think you can assoc into a vector to replace a value at an index:
  (assoc [1 2 3 4] 2 9) ;; => [1 2 9 4]
  ;; so we use this to perform our update:
  (let [len (count test-update)]
    (reduce
     (fn [v [i j]]
       (let [ni (nth test-update i)
             nj (nth test-update j)]
         (if
          (contains?
           (get-in test-input [:rules ni])
           nj
           (-> v
               (assoc i nj)
               (assoc j ni)
               v)))))
     test-update
     (for [i (range len)
           j (range len)
           :when (< i j)]
       [i j])))
  ;; test update before: ["97" "13" "75" "29" "47"]
  ;; test update after:  ["97" "47" "13" "47" "29"]
  ;; expected result:    [ 97,  75,  47,  29,  13 ]

  ;; hm so this one actually does require multiple swaps:
  ;; [ 97,  13,  75,  29,  47 ]
  ;; 75|13 => [ 97,  75,  13,  29,  47 ]
  ;; 29|13 => [ 97,  75,  29,  13,  47 ]
  ;; 47|29 => [ 97,  75,  47,  13,  29 ]
  ;; 29|13 => [ 97,  75,  47,  29,  13 ]
  ;; so the algorithm will actually yield the right result for this particular input
  ;; without modification.
  (let [len (count test-update)]
    (reduce
     (fn [v [i j]]
       (let [ni (nth (:r v) i)
             nj (nth (:r v) j)]
         (if
          (contains?
           (get-in test-input [:rules ni])
           nj
           (let [v' (-> (:r v)
                        (assoc i nj)
                        (assoc j ni))
                 log (str (format "[%d %d] => [%s %s]: Rule found, update vector is now "
                                  i j ni nj) v')]
             (-> v
                 (assoc :r v')
                 (update :logs #(conj % log))))
           v))))
     {:r test-update :logs []}
     (for [i (range len)
           j (range len)
           :when (< i j)]
       [i j])))
  ;; okay so it does work after all, i just forgot to update some of the vars at the top
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
                     nj
                     (-> v
                         (assoc i nj)
                         (assoc j ni))
                     v))))
               update
               (for [i (range len)
                     j (range len)
                     :when (< i j)]
                 [i j])))))))
  (solve-pt2 test-input)
  ;; => (["97" "75" "47" "61" "53"] ["61" "29" "13"] ["97" "75" "47" "29" "13"])
  ;; that's correct!
  ;; this code is heavily duplicated but that's alright for now. A good optional improvement
  ;; for later :)
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
                     nj
                     (-> v
                         (assoc i nj)
                         (assoc j ni))
                     v))))
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
  (solve-pt2 test-input)
  ;; => 123
  ;; okay, let's try it on the real input:
  (solve-pt2 input)
  ;; => 4971
  ;; accepted! i'm surprised, honestly, but i'll take it
  ())
