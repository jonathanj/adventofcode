(ns advent2017.day5
  "Mutate the input repeatedly after each jump, with only the mutation differing
  between part 1 and part 2."
  (:require [advent.core :refer [->lines ->int]]
            [advent2017.core :refer [read-puzzle]]))

(def puzzle (read-puzzle "day5.data" (comp vec
                                           (partial map ->int)
                                           ->lines)))

(defn solve [inc-fn input]
  (loop [input' (transient input)
         pos    0
         jumps  1]
    (let [incr          (nth input' pos)
          ^long new-pos (+ pos incr)]
      (if (>= new-pos (count input))
        jumps
        (recur (assoc! input' pos (inc-fn incr))
               new-pos
               (inc jumps))))))

(def solve-1 (partial solve inc))
(def solve-2 (partial solve (fn [^long n] (if (>= n 3) (dec n) (inc n)))))

(comment
  (assert (= (solve-1 [0 3 0 1 -3]) 5))
  (solve-1 puzzle)

  (assert (= (solve-2 [0 3 0 1 -3]) 10))
  (solve-2 puzzle))
