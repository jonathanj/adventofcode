(ns advent2017.day17
  "Part 1 is the direct implementation, spinning and inserting into an array.
  Part 2 cheats a bit and only tracks the value in position 1 (the position
  after 0) until we hit 50 million loops.")

(def sample-puzzle 3)
(def puzzle 354)

(defn solve-1 [^long input]
  (loop [res (java.util.ArrayList. [0])
         idx 0
         n   1]
    (if (> n 2017)
      (nth (vec res) (rem (inc idx) n))
      (let [new-idx (inc (rem (+ idx input) n))]
        (recur (doto res (.add new-idx n))
               new-idx
               (inc n))))))

(defn solve-2 [^long input]
  (loop [pos 0
         n   1
         res nil]
    (if (== n 50000001)
      res
      (let [new-pos (inc (rem (+ pos input) n))]
        (recur new-pos
               (inc n)
               (if (== 1 new-pos)
                 n
                 res))))))

(comment
  (assert (= (solve-1 sample-puzzle) 638))
  (solve-1 puzzle)

  (solve-2 puzzle))
