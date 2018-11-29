(ns advent2017.day3
  "The general idea here is to construct a set of instructions (1 up, 2 left,
  etc.) that forms a spiral. In this case of a spiral the steps are: 1 right, 1
  up, 2 left, 2 down, 3 right, etc. At each step the next step is calculated
  from the current state.

  For part 1 the next step is just adding 1 to the last step. For part 2 the
  next step is the sum of the adjacent neighbours of the new spiral position.

  http://adventofcode.com/2017/day/3"
  (:require [advent.core :refer [manhattan-distance]]))


(def spiral (mapcat (fn [n [a b]] [[n a] [n b]])
                    (drop 1 (range))
                    (cycle [[:R :U] [:L :D]])))

(defn neighbours [[x y]]
  [[(dec x) (dec y)]
   [x (dec y)]
   [(inc x) (dec y)]
   [(inc x) y]
   [(inc x) (inc y)]
   [x (inc y)]
   [(dec x) (inc y)]
   [(dec x) y]])

(defn one-instruction [[route [x y]] [n dir] pred next-step]
  (let [next-pos (case dir
                   :R [(inc x) y]
                   :U [x (dec y)]
                   :L [(dec x) y]
                   :D [x (inc y)])
        step     (next-step [route [x y] next-pos])]
    (if (or (not (pred [route [x y]]))
            (zero? n))
      [route [x y]]
      (recur [(assoc route next-pos step) next-pos]
             [(dec n) dir]
             pred
             next-step))))

(defn solve [pred next-step]
  (loop [result        [{[0 0] 1} [0 0]]
         [inst & path] spiral]
    (if-not (pred result)
      result
      (recur (one-instruction result inst pred next-step) path))))

(defn solve-1 [n]
  (let [next-step (fn [[route pos _]] (inc (route pos)))
        pred      (fn [[route pos]] (< (route pos) n))]
    (manhattan-distance (last (solve pred next-step)))))

(defn solve-2 [n]
  (let [next-step (fn [[route _ pos]]
                    (apply + (remove nil? (map route (neighbours pos)))))
        pred      (fn [[route pos]] (<= (route pos) n))
        [route pos] (solve pred next-step)]
    (route pos)))

(comment
  (assert (= (solve-1 1) 0))
  (assert (= (solve-1 12) 3))
  (assert (= (solve-1 1024) 31))
  (solve-1 361527)

  (assert (= (solve-2 1) 2))
  (assert (= (solve-2 4) 5))
  (assert (= (solve-2 26) 54))
  (solve-2 361527))
