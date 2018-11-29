(ns advent2017.day23
  "Part 1 is a slightly modified version of day 18. Part 2 is reimplementing the
  program, which counts the number of composite numbers within some range."
  (:require [advent.core :refer [->lines]]
            [advent2017.core :refer [read-puzzle]]
            [advent2017.day18 :refer [compile-instruction
                                      registers
                                      execute
                                      binary-op
                                      instruction-set-1]]))

(def instruction-set
  (merge instruction-set-1
         {"sub" (binary-op -)
          "jnz" (fn [[_ a'] [_ b']]
                  (fn [reg _]
                    (update reg :ip + (if-not (zero? (a' reg)) (b' reg) 1))))}))

(def puzzle (read-puzzle "day23.data" ->lines))

(def solve #(->> %
                 (mapv (partial compile-instruction instruction-set))
                 (execute registers nil)))

(defn solve-1 [input]
  (get-in (solve input) [:counts "mul"]))

(defn solve-2 [input]
  (let [b (+ 100000 (* 100 ((solve input) "b")))]
    (count
     (filter (fn [n]
               (some #(zero? (rem n %)) (range 2 (int (Math/sqrt n)))))
             (range b (+ b 17001) 17)))))

(comment
  (solve-1 puzzle)
  (solve-2 puzzle))

