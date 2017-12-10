(ns advent2017.day10
  "Implement a single round iteration by replacing affected elements one at a
  time forwards, with the reverse of the subring. Implement multiple rounds by
  repeating the input lengths n times."
  (:require [advent2017.core :refer [read-puzzle ->lines csv->numbers]]))

(def puzzle (read-puzzle "day10.data" (comp first ->lines)))

(defn subring [ring start length]
  (let [size (count ring)
        end  (mod (+ start length) size)]
    (if (> end start)
      ;; No wrapping is taking place, so just the simple subvec.
      (subvec ring start end)
      ;; Implement wrapping by joining two subvecs.
      (vec (concat (subvec ring start)
                   (subvec ring 0 end))))))

(defn reverse-subring [ring start length]
  (let [size (count ring)]
    (if (<= length 1)
      ring
      (loop [res      ring
             n        0
             [x & xs] (reverse (subring ring start length))]
        (if-not x
          res
          (recur (assoc res (mod (+ start n) size) x)
                 (inc n)
                 xs))))))

(defn rounds
  ([input n]
   (rounds (vec (range 256)) input n))
  ([ring input n]
   (let [size (count ring)]
     (loop [res      ring
            [l & ls] (apply concat (repeat n input))
            pos      0
            skip     0]
       (if-not l
         res
         (recur (reverse-subring res pos l)
                ls
                (mod (+ pos l skip) size)
                (inc skip)))))))

(defn dense [xs]
  (map #(apply bit-xor %) (partition 16 xs)))

(defn bytes->hex [xs]
  (apply str (map #(format "%02x" %) xs)))

(defn solve-1 [ring input]
  (->> (rounds ring (vec (csv->numbers input)) 1)
       (take 2)
       (apply *)))

(defn solve-2 [input]
  (-> (map byte input)
      (concat (list 17 31 73 47 23))
      (vec)
      (rounds 64)
      (dense)
      (bytes->hex)))

(comment
  (assert (= (solve-1 [0 1 2 3 4] "3,4,1,5") 12))
  (solve-1 (vec (range 256)) puzzle)

  (assert (= (solve-2 "") "a2582a3a0e66e6e86e3812dcb672a272"))
  (assert (= (solve-2 "AoC 2017") "33efeb34ea91902bb2f59c9920caa6cd"))
  (solve-2 puzzle))
