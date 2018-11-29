(ns advent2017.day24
  "By definining a successors function that returns the cumulative strength and
  length of a path we can solve part 1 by finding the maximimum strength value
  and part 2 by finding the strength of the maximum length value."
  (:require [loom.alg-generic :refer [bf-span]]
            [advent.core :refer [->lines ->int]]
            [advent2017.core :refer [read-puzzle]]))

(defn lines->puzzle [lines]
  (set (map (fn [s] (mapv ->int (clojure.string/split s #"/"))) lines)))

(def sample-puzzle (lines->puzzle ["0/2" "2/2" "2/3" "3/4" "3/5" "0/1" "10/1" "9/10"]))
(def puzzle (read-puzzle "day24.data" (comp lines->puzzle ->lines)))

(defn neighbours [[^long t ^long c ^long m available]]
  (for [[^long a ^long b :as p] available
        :let                    [a? (== m a)
                                 n  (if a? b a)]
        :when                   (or a? (== m b))]
    [(+ t m n) (inc c) n (disj available p)]))

(defn solve [input]
  (apply concat (vals (bf-span neighbours [0 0 0 input]))))

(defn solve-1 [input]
  (apply max (map first (solve input))))

(defn solve-2 [input]
  (first (apply max-key second (solve input))))

(comment
  (assert (= (solve-1 sample-puzzle) 31))
  (solve-1 puzzle)

  (assert (= (solve-2 sample-puzzle) 19))
  (solve-2 puzzle))
