(ns advent2018.day3
  "https://adventofcode.com/2018/day/3"
  (:require [advent.core :refer [->lines ->int]]
            [advent2018.core :refer [read-puzzle]]))

(defn parse-claim [s]
  (let [[_ id x y w h] (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s)
        x1 (->int x)
        y1 (->int y)]
    {:id id
     :x1 x1
     :y1 y1
     :x2 (+ x1 (->int w))
     :y2 (+ y1 (->int h))}))

(def puzzle (read-puzzle "day3" (comp (partial map parse-claim)
                                      ->lines)))

(defn mark [^ints fabric {:keys [x1 y1 x2 y2]}]
  (doseq [x    (range (- x2 x1))
          y    (range (- y2 y1))
          :let [idx (+ x1 x (* (+ y1 y) 1000))
                v (aget fabric idx)]]
    (aset fabric idx (inc v))))

(defn solve-1
  [input]
  (let [fabric (int-array (* 1000 1000) 0)]
    (doseq [c input]
      (mark fabric c))
    (count (filter #(> % 1) fabric))))

(defn intersects? [a b]
  (and (< (:x1 a) (:x2 b))
       (> (:x2 a) (:x1 b))
       (< (:y1 a) (:y2 b))
       (> (:y2 a) (:y1 b))))

(defn solve-2
  [input]
  (let [xs (transient (set (map :id input)))]
    (doseq [a input
            b input
            :when (and (not= a b)
                       (intersects? a b))]
      (disj! xs (:id a) (:id b))
      )
    (persistent! xs)))

(comment
  (def sample-1 (map parse-claim ["#1 @ 1,3: 4x4"
                                  "#2 @ 3,1: 4x4"
                                  "#3 @ 5,5: 2x2"]))

  (assert (= (solve-1 sample-1) 4))
  (solve-1 puzzle)

  (assert (= (solve-2 sample-1) #{"3"}))
  (solve-2 puzzle))
