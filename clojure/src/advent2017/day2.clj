(ns advent2017.day2
  "http://adventofcode.com/2017/day/2"
  (:require [advent.core :refer [lines->numbers ->lines]]
            [advent2017.core :refer [read-puzzle]]))

(def puzzle (read-puzzle "day2.data" lines->numbers))

(defn solve [f rows]
  (apply + (map f rows)))

(defn diff-min-max [row]
  (- (apply max row)
     (apply min row)))

(defn divisible-pair [row]
  (for [a     row
        b     row
        :when (and (not= a b)
                   (zero? (rem a b)))]
    (/ a b)))

(def solve-1 (partial solve diff-min-max))
(def solve-2 (partial solve (comp first divisible-pair)))

(comment
  (assert (= (solve-1 [[5 1 9 5] [7 5 3] [2 4 6 8]]) 18))
  (solve-1 puzzle)

  (assert (= (solve-2 [[5 9 2 8] [9 4 7 3] [3 8 6 5]]) 9))
  (solve-2 puzzle))
