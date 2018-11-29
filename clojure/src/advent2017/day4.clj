(ns advent2017.day4
  "Part 1 is simply finding duplicate words, we can compare the length of the
  original list of words with the length of a set of the original list, if there
  are any duplicates the lengths will be unequal.

  Part 2 requires checking anagrams. For two words to be anagrams all we need to
  do is compare letter frequencies.

  http://adventofcode.com/2017/day/4"
  (:require [advent.core :refer [->words ->lines]]
            [advent2017.core :refer [read-puzzle]]))

(def puzzle (read-puzzle "day4.data" ->lines))

(defn solve [xform lines]
  (reduce (fn [counter words]
            (if (= (count words)
                   (count (set words)))
              (inc counter)
              counter))
          0
          (map (comp xform ->words) lines)))

(def solve-1 (partial solve identity))
(def solve-2 (partial solve frequencies))

(comment
  (assert (= (solve-1 ["aa bb cc dd ee"]) 1))
  (assert (= (solve-1 ["aa bb cc dd aa"]) 0))
  (assert (= (solve-1 ["aa bb cc dd aaa"]) 1))
  (solve-1 puzzle)

  (assert (= (solve-2 ["abcde fghij"]) 1))
  (assert (= (solve-2 ["abcde xyz ecdab"]) 0))
  (assert (= (solve-2 ["a ab abc abd abf abj"]) 1))
  (assert (= (solve-2 ["iiii oiii ooii oooi oooo"]) 1))
  (assert (= (solve-2 ["oiii ioii iioi iiio"]) 0))
  (solve-2 puzzle))
