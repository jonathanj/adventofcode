(ns advent2018.day6
  "https://adventofcode.com/2018/day/6"
  (:require [clojure.string :as str]
            [advent.core :refer [->lines ->coordinates manhattan-distance]]
            [advent2018.core :refer [read-puzzle]]))

(def parse-puzzle (comp (partial map ->coordinates)
                        ->lines))

(def puzzle (read-puzzle "day6" parse-puzzle))

(defn bounding [coords]
  (let [fs (map first coords)
        ss (map second coords)]
    [(apply min fs)
     (apply max fs)
     (apply min ss)
     (apply max ss)]))

(defn out-of-bounds? [[ax bx ay by] [x y]]
  (or (<= x ax)
      (>= x bx)
      (<= y ay)
      (>= y by)))

(defn areas [bounds res]
  (frequencies (for [x     res
                     :let  [[d loc] x]
                     :when (not (out-of-bounds? bounds loc))]
                 loc)))

(defn solve-1
  "For each coordinate, within the bounds of the input, calculate the two
  nearest points from the input. Discard points that are equidistant and keep
  only the nearest point. Then calculate the area occupied for each point and
  find only the largest area for the point that is within the bounds."
  [input]
  (let [bounds        (bounding input)
        [min-x max-x
         min-y max-y] bounds]
    (->> (for [y     (range min-y max-y)
               x     (range min-x max-x)
               :let  [loc   [x y]
                      distf (partial manhattan-distance loc)
                      dists (sort-by first (map (juxt distf identity) input))
                      [a b] (take 2 dists)]
               :when (not (= (first a) (first b)))]
           a)
         (areas bounds)
         (vals)
         (sort-by -)
         (first))))

(defn solve-2
  "For each coordinate calculate the cumulative distance to every input
  coordinate, discarding results greater than `max-dist`. Count the number of
  results."
  [input max-dist]
  (let [bounds        (bounding input)
        [min-x max-x
         min-y max-y] bounds]
    (count (for [y     (range min-y max-y)
                 x     (range min-x max-x)
                 :let  [loc   [x y]
                        distf (partial manhattan-distance loc)
                        dist (apply + (map distf input))]
                 :when (< dist max-dist)]
             dist))))

(comment
  (def sample (parse-puzzle (str/join "\n" ["1, 1"
                                            "1, 6"
                                            "8, 3"
                                            "3, 4"
                                            "5, 5"
                                            "8, 9"])))
  (assert (= (solve-1 sample) 17))
  (solve-1 puzzle)

  (assert (= (solve-2 sample 32) 16))
  (solve-2 puzzle 10000))
