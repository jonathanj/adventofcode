(ns advent2017.day22
  ""
  (:require [advent.core :refer [->lines]]
            [advent2017.core :refer [read-puzzle]]))

(def directions
  {:L [-1  0]
   :U [ 0 -1]
   :R [ 1  0]
   :D [ 0  1]})

(def turns
  {:clean    {:L :D
              :D :R
              :R :U
              :U :L}
   :infected {:L :U
              :D :L
              :R :D
              :U :R}
   :weakened {:L :L
              :D :D
              :R :R
              :U :U}
   :flagged  {:L :R
              :D :U
              :R :L
              :U :D}})

(defn move [act {:keys [^longs pos dir grid ^long infected] :as state}]
  (let [s       (grid pos :clean)
        ns      (act s)
        new-dir (dir (turns s))]
    (assoc! state
           :pos (mapv + pos (directions new-dir))
           :dir new-dir
           :grid (assoc! grid pos ns)
           :infected (+ infected (if (= :infected ns) 1 0)))))

(defn lines->grid [lines]
  (let [lines (vec lines)
        w     (count (first lines))
        h     (count lines)
        cx    (int (/ w 2))
        cy    (int (/ h 2))]
    (transient
     {:pos      [cx cy]
      :dir      :U
      :infected 0
      :grid     (transient
                 (into {} (for [^long x (range 0 w)
                                ^long y (range 0 h)]
                            [[x y]
                             (if (= \# (nth (nth lines y) x)) :infected :clean)])))})))

(def sample-puzzle (lines->grid ["..#"
                                 "#.."
                                 "..."]))
(def puzzle (read-puzzle "day22.data" (comp lines->grid ->lines)))

(defn solve [n act input]
  (->> (iterate (partial move act) input)
       (drop n)
       (first)
       :infected))

(def solve-1 (partial solve
                      10000
                      {:clean    :infected
                       :infected :clean}))
(def solve-2 (partial solve
                      10000000
                      {:clean    :weakened
                       :weakened :infected
                       :infected :flagged
                       :flagged :clean}))

(comment
  (assert (= (solve-1 sample-puzzle) 5587))
  (solve-1 puzzle)

  (assert (= (solve-2 sample-puzzle) 2511944))
  (solve-2 puzzle))
