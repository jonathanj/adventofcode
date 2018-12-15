(ns advent2018.day11
  (:require [advent.core :refer [->int]]
            [advent2018.core :refer [read-puzzle]]))

(defn third-last [^String s]
  (if-let [^Character h (get s (- (count s) 3))]
    (Character/digit h 10)
    0))

(defn power-level [serial-number [x y]]
  (let [rack-id (+ x 10)]
    (-> rack-id
        (* y)
        (+ serial-number)
        (* rack-id)
        (str)
        (third-last)
        (- 5))))

;; https://en.wikipedia.org/wiki/Summed-area_table
(defn magic-square [^doubles grid [x y n]]
  (let [idx (+ x (* y 300))
        tl (aget grid (+ idx -301))
        br (aget grid (+ idx (dec n) (* 300 (dec n))))
        tr (aget grid (+ idx -300 (dec n)))
        bl (aget grid (+ idx (dec (* 300 (dec n)))))]
    (- (+ tl br) tr bl)))

(defn find-max [grid n]
  (apply max-key
         first
         (for [x    (range 1 (- 300 n))
               y    (range 1 (- 300 n))
               :let [coord [x y n]]]
           [(magic-square grid [x y n])
            ;; Adjust for base-1 coordinate output.
            [(inc x) (inc y) n]])))

(defn solve [[n m] grid]
  (let [work   (partition-all 30 (range n (inc m)))
        f      (partial find-max grid)
        result (pmap (fn [xs] (doall (map f xs))) work)]
    (second (apply max-key
                   first
                   (apply concat result)))))

(defn cumsum [grid]
  (->> grid
       (map (partial reductions +))
       (reductions (partial map +))))

(defn make-grid [serial-number]
  (->> (for [y (range 300)
             x (range 300)]
         (power-level serial-number [(inc x) (inc y)]))
       (partition-all 300)
       (cumsum)
       (apply concat)
       (double-array)))

(def puzzle (read-puzzle "day11" (comp make-grid ->int)))

(def solve-1 (comp vec
                   butlast
                   (partial solve [3 3])))
(def solve-2 (comp (partial solve [3 298])))

(comment
  (assert (= (solve-1 (make-grid 18)) [33 45]))
  (assert (= (solve-1 (make-grid 42)) [21 61]))
  (solve-1 puzzle)

  (assert (= (solve-2 (make-grid 18)) [90 269 16]))
  (assert (= (solve-2 (make-grid 42)) [232 251 12]))
  (solve-2 puzzle))
