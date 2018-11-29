(ns advent2017.day10
  "Implement a single round iteration by replacing affected elements one at a
  time forwards, with the reverse of the subring. Implement multiple rounds by
  repeating the input lengths n times."
  (:require [advent.core :refer [->lines csv->numbers]]
            [advent2017.core :refer [read-puzzle]]))

(def puzzle (read-puzzle "day10.data" (comp first ->lines)))

(defn reverse-subring [^ints ring ^long size ^long start ^long length]
  (if (<= length 1)
    ring
    (loop [res ring
           a   start
           b   (rem (+ start (dec length)) size)
           n   (int (/ length 2))]
      (if (== n 0)
        res
        (let [av (aget res a)
              bv (aget res b)]
          (aset res a bv)
          (aset res b av)
          (recur res
                 (rem (inc a) size)
                 (mod (dec b) size)
                 (dec n)))))))

(defn rounds
  ([input n]
   (rounds (vec (range 256)) input n))
  ([ring input n]
   (let [size       (count ring)
         input-size (count input)]
     (loop [res      (int-array ring)
            [l & ls] (apply concat (repeat n input))
            pos      0
            skip     0]
       (if-not l
         (vec res)
         (recur (reverse-subring res size pos l)
                ls
                (rem (+ pos l skip) size)
                (inc skip)))))))

(defn dense [xs]
  (map #(apply bit-xor %) (partition 16 xs)))

(defn bytes->hex [xs]
  (apply str (map #(format "%02x" %) xs)))

(defn solve-1 [ring input]
  (->> (rounds ring (vec (csv->numbers input)) 1)
       (take 2)
       (apply *)))

(defn knot-hash [input]
  (-> (mapv int input)
      (conj 17 31 73 47 23)
      (rounds 64)
      (dense)))

(defn solve-2 [input]
  (bytes->hex (knot-hash input)))

(comment
  (assert (= (solve-1 [0 1 2 3 4] "3,4,1,5") 12))
  (solve-1 (vec (range 256)) puzzle)

  (assert (= (solve-2 "") "a2582a3a0e66e6e86e3812dcb672a272"))
  (assert (= (solve-2 "AoC 2017") "33efeb34ea91902bb2f59c9920caa6cd"))
  (solve-2 puzzle))
