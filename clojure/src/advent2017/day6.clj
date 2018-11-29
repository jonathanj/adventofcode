(ns advent2017.day6
  "Recursively redistribute the largest memory banks until we reach a state
  we've seen before. Part 1 ends here, the result is the number of cycles
  required to reach a previously seen state from the initial state. Part 2 feeds
  the previously seen state into part 1 to determine the size of the cycle
  loop."
  (:require [advent.core :refer [->words ->numbers]]
            [advent2017.core :refer [read-puzzle]]))

(def puzzle (read-puzzle "day6.data" (comp ->numbers ->words)))

(defn redistribute [memory [idx m]]
  (loop [memory (assoc memory idx 0)
         idx    (mod (inc idx) (count memory))
         m      m]
    (if (zero? m)
      memory
      (recur (update memory idx inc)
             (mod (inc idx) (count memory))
             (dec m)))))

(defn max-bank [memory]
  (let [m (apply max memory)]
    [(.indexOf memory m) m]))

(defn solve [input]
  (loop [memory input
         seen   #{}
         count  0]
    (if (seen memory)
      [count memory]
      (recur (redistribute memory (max-bank memory))
             (conj seen memory)
             (inc count)))))

(def solve-1 (comp first solve))
(def solve-2 (comp solve-1 second solve))

(comment
  (assert (= (solve-1 [0 2 7 0]) 5))
  (solve-1 puzzle)

  (assert (= (solve-2 [0 2 7 0]) 4))
  (solve-2 puzzle))
