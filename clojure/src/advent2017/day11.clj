(ns advent2017.day11
  "Define directions as cube-coordinate offsets on a hex grid and a distance
  function in cube-coordinates. The directions can then be navigated by reducing
  the offsets. Part 1 is the distance from the final position to the origin.
  Part 2 is the intermediate step with the largest distance."
  (:require [advent2017.core :refer [read-puzzle ->csv ->lines]]))

;; https://www.redblobgames.com/grids/hexagons/

(def puzzle (read-puzzle "day11.data" (comp ->csv first ->lines)))

(defn cube-distance
  ([[dx dy dz]]
   (cube-distance [0 0 0] [dx dy dz]))
  ([[sx sy sz] [dx dy dz]]
   (/ (+ (Math/abs (- sx dx))
         (Math/abs (- sy dy))
         (Math/abs (- sz dz))) 2)))

(defn step [pos off] (mapv + pos off))

(def dir->offset
  {"se" [(+ 1) (- 1)   0  ]
   "ne" [(+ 1)   0   (- 1)]
   "n"  [  0   (+ 1) (- 1)]
   "nw" [(- 1) (+ 1)   0  ]
   "sw" [(- 1)   0   (+ 1)]
   "s"  [  0   (- 1) (+ 1)]})

(defn navigation [input]
  (reductions step [0 0 0] (map dir->offset input)))

(def solve-1 (comp cube-distance last navigation))
(def solve-2 (comp (partial apply max)
                   (partial map cube-distance)
                   navigation))

(comment
  (assert (= (solve-1 ["ne" "ne" "ne"]) 3))
  (assert (= (solve-1 ["ne" "ne" "sw" "sw"]) 0))
  (assert (= (solve-1 ["ne" "ne" "s" "s"]) 2))
  (solve-1 puzzle)

  (solve-2 puzzle))
