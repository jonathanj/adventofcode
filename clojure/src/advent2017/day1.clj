(ns advent2017.day1
  "Part 1 requires producing the sum where the next digit is the equal to the
  current digit, with wraparound.

  Part 2 requires producing the sum where the digit half-way around is equal to
  the current digit, with wraparound.

  http://adventofcode.com/2017/day/1"
  (:require [advent.core :refer [enumerate]]
            [advent2017.core :refer [read-puzzle]]))

(def puzzle (read-puzzle "day1.data"))

(defn char->int [c]
  (- (int c) (int \0)))

(defn solve [increment input]
  (let [incr (increment input)]
    (apply + (for [[n c]  (enumerate input)
                   :when (= c (nth input (mod (+ n incr) (count input))))]
                (char->int c)))))

(def solve-1 (partial solve (constantly 1)))
(def solve-2 (partial solve #(/ % 2)))

(comment
  (assert (= (solve-1 "1122") 3))
  (assert (= (solve-1 "1111") 4))
  (assert (= (solve-1 "1234") 0))
  (assert (= (solve-1 "91212129") 9))
  (solve-1 puzzle)

  (assert (= (solve-2 "1212") 6))
  (assert (= (solve-2 "1221") 0))
  (assert (= (solve-2 "123425") 4))
  (assert (= (solve-2 "123123") 12))
  (assert (= (solve-2 "12131415") 4))
  (solve-2 puzzle))
