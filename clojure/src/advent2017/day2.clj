(ns advent2017.day2
  (:require [advent2017.core :refer [lines->numbers]]))

(def puzzle (-> (clojure.java.io/reader "../2017/day2.data")
                (line-seq)
                (lines->numbers)))

(defn solve-1 [rows]
  (apply + (for [row rows]
             (- (apply max row) (apply min row)))))

(defn divisible-pair [row]
  (for [a     row
        b     row
        :when (and (not= a b)
                   (zero? (rem a b)))]
    (/ a b)))

(defn solve-2 [rows]
  (apply + (for [row rows]
             (first (divisible-pair row)))))

(comment
  (assert (= (solve-1 [[5 1 9 5] [7 5 3] [2 4 6 8]]) 18))
  (solve-1 puzzle)

  (assert (= (solve-2 [[5 9 2 8] [9 4 7 3] [3 8 6 5]]) 9))
  (solve-2 puzzle))
