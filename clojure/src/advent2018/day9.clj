(ns advent2018.day9
  (:require [advent.core :refer [->numbers]]
            [advent2018.core :refer [read-puzzle]]))

(def puzzle (read-puzzle "day9" (comp ->numbers
                                      (partial map second)
                                      (partial re-seq #"(\d+)"))))

(defn cw2 [circle ^long index]
  (let [res  (unchecked-add index 2)
        size (count circle)]
    (if (> res size)
      (unchecked-subtract-int res size)
      res)))

(defn ccw7 [circle ^long index]
  (let [res  (unchecked-subtract-int index 7)
        size (count circle)]
    (if (neg? res)
      (unchecked-add size res)
      res)))

(defn new-game [player-count]
  {:player       0
   :player-count player-count
   :marble       1
   :index        1
   :circle       (java.util.ArrayList. [0 1])
   :scores       (vec (repeat (inc player-count) 0))})

(defn one-round [game ^long marble]
  (let [{:keys [^long player
                ^long player-count
                #_^long marble
                ^long index
                ^long lulz
                scores
                ^java.util.ArrayList circle]} game]
    (let [marble (inc marble)
          player (inc (mod player player-count))
          game   (assoc! game :marble marble :player player)]
      (if (zero? (mod marble 23))
        (let [^long index (ccw7 circle index)
              previous    (.remove circle index)]
          (assoc! game
                  :index index
                  :scores (update scores player + marble previous)))
        (let [index (cw2 circle index)]
          (.add circle index marble)
          (assoc! game :index index))))))

(defn solve-1 [^long p ^long n]
  (apply max (:scores (reduce (fn [game n]
                                (one-round game n))
                              (transient (new-game p))
                              (range 1 n))))

  #_(apply max (:scores (nth (iterate one-round (transient (new-game p))) (dec n)))))

(defn solve-2 [^long p ^long n]
  ;; XXX: Implement this with a circular doubly linked list instead of this,
  ;; it's really slow.
  (solve-1 p (* 100 n)))

(comment
  (assert (= (solve-1 9 25) 32))
  (assert (= (solve-1 10 1618) 8317))
  (assert (= (solve-1 13 7999) 146373))
  (assert (= (solve-1 17 1104) 2764))
  (assert (= (solve-1 21 6111) 54718))
  (assert (= (solve-1 30 5807) 37305))
  (apply solve-1 puzzle)

  (apply solve-2 puzzle))
