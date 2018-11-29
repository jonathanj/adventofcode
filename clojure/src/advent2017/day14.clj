(ns advent2017.day14
  ""
  (:require [loom.graph :refer [graph]]
            [loom.alg :refer [connected-components]]
            [advent.core :refer [num->binary]]
            [advent2017.core :refer [read-puzzle]]
            [advent2017.day10 :refer [knot-hash]]))

(def sample-puzzle "flqrgnkx")
(def puzzle "wenycdww")

(defn adjacency [[input [w h]] [x y]]
  (letfn [(v [x y] (nth input (+ x (* w y))))]
    (if (= \0 (v x y))
      nil
      {[x y] (conj
              (set (filter (fn [[dx dy]] (and (< -1 dx w)
                                              (< -1 dy h)
                                              (= \1 (v dx dy))))
                           [[x (dec y)]
                            [(inc x) y]
                            [x (inc y)]
                            [(dec x) y]]))
              [x y])})))

(defn hash->binary
  ([w] "")
  ([w hash]
   (-> (reduce (fn [s n] (str s (num->binary n 8))) "" hash)
       (subs 0 w))))

(defn generate-grid
  ([input]
   (generate-grid input [128 128]))
  ([input [w h]]
   [(->> (mapv #(str %1 "-" %2) (repeat input) (range h))
         (pmap (comp (partial hash->binary w) knot-hash))
         (apply str))
    [w h]]))

(defn grid->graph [[_ [w h] :as grid]]
  (apply graph (->> (vec (for [x (range w) y (range h)] [x y]))
                    (map (partial adjacency grid))
                    (remove nil?))))


(def solve-1 (comp count
                   (partial filter #(= \1 %))
                   first
                   generate-grid))

(def solve-2 (comp count
                   connected-components
                   grid->graph
                   generate-grid))

(comment
  (assert (= (solve-1 sample-puzzle) 8108))
  (solve-1 puzzle)

  (assert (= (solve-2 sample-puzzle) 1242))
  (solve-2 puzzle))
