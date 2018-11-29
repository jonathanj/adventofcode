(ns advent2017.day21
  ""
  (:require [clojure.string :refer [join split]]
            [advent.core :refer [->lines]]
            [advent2017.core :refer [read-puzzle]]))

(defn rotate [x]
  (vec (apply map (comp join vector) (reverse x))))

(defn flip-h [x]
  (mapv (comp join reverse) x))

(defn flip-v [x]
  (mapv join (reverse x)))

(defn combinations [x]
  (let [rots (take 4 (iterate rotate x))]
    (concat
     rots
     (map flip-v rots)
     (map flip-h rots)
     (map (comp flip-v flip-h) rots))))

(defn replacement-pattern [p r]
  (zipmap
   (map join (combinations p))
   (repeat (join r))))

(defn ->bits [s]
  (split s #"/"))

(defn parse [lines]
  (apply merge
         (map (fn [s]
                (let [[_ p r] (re-find #"^(.+?) => (.+?)$" s)]
                  (replacement-pattern (->bits p) (->bits r))))
              lines)))

(def start [(join (->bits ".#./..#/###")) 3])
(def sample-puzzle (parse ["../.# => ##./#../..."
                         ".#./..#/### => #..#/..../..../#..#"]))
(def puzzle (read-puzzle "day21.data" (comp parse ->lines)))

(defn slice-2 [[grid size] [^long x ^long y]]
  (let [idx0 (+ x (* y size))
        idx1 (inc idx0)
        idx2 (+ idx0 size)
        idx3 (inc idx2)]
    (str (nth grid idx0)
         (nth grid idx1)
         (nth grid idx2)
         (nth grid idx3))))

(defn slice-3 [[grid ^long size] [^long x ^long y]]
  (let [idx0 (+ x (* y size))
        idx1 (inc idx0)
        idx2 (inc idx1)
        idx3 (+ idx0 size)
        idx4 (inc idx3)
        idx5 (inc idx4)
        idx6 (+ idx3 size)
        idx7 (inc idx6)
        idx8 (inc idx7)]
    (str (nth grid idx0)
         (nth grid idx1)
         (nth grid idx2)
         (nth grid idx3)
         (nth grid idx4)
         (nth grid idx5)
         (nth grid idx6)
         (nth grid idx7)
         (nth grid idx8))))

(defn slice [[grid ^long size] n]
  (let [ps (for [^long y (range 0 size n)
                 ^long x (range 0 size n)]
             [x y])
        f  (partial (case n
                      2 slice-2
                      3 slice-3) [grid size])]
    (map f ps)))

(defn join-grid [pieces row-size block-size]
  (let [ppr (quot row-size block-size)]
    (apply concat (for [p (partition ppr pieces)]
                    (let [x (map #(partition block-size %) p)]
                      (apply mapcat concat x))))))

(defn enhance [input [blocks ^long size :as grid]]
  (let [r        (map input (cond
                              (zero? (rem size 2)) (slice grid 2)
                              (zero? (rem size 3)) (slice grid 3)))
        bs       (long (Math/sqrt (count (first r))))
        new-size (* bs (long (Math/sqrt (count r))))]
    [(join (join-grid r new-size bs)) new-size]))

(defn solve [n input]
  (->> (iterate (partial enhance input) start)
       (drop n)
       (ffirst)
       (filter #{\#})
       (count)))

(comment
  (assert (= (solve 2 sample-puzzle) 12))
  (solve 5 puzzle)

  (solve 18 puzzle))

