(ns advent2018.day1
  "https://adventofcode.com/2018/day/1"
  (:require [advent.core :refer [->lines ->numbers]]
            [advent2018.core :refer [read-puzzle]]))

(def puzzle (read-puzzle "day1" (comp ->numbers ->lines)))

(def solve-1 (partial apply +))
(defn solve-2 [xs]
  (loop [seen     #{0}
         acc      0
         [x & xs] (cycle xs)]
    (let [res (+ acc x)]
      (if (seen res)
        res
        (recur (conj seen res) res xs)))))

(comment
  (assert (= (solve-1 [1 1 1]) 3))
  (assert (= (solve-1 [1 1 -2]) 0))
  (solve-1 puzzle)

  (assert (= (solve-2 [1 -1]) 0))
  (assert (= (solve-2 [3 3 4 -2 -4]) 0))
  (solve-2 puzzle)
  )
