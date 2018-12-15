(ns advent2018.day8
  (:require [advent.core :refer [->numbers ->words]]
            [advent2018.core :refer [read-puzzle]]))


(defn read-tree [input]
  (letfn [(read-node [[c m & input]]
            (let [[children input] (nth (iterate read-child [[] input]) c)]
              [(cons (take m input) children) (drop m input)]))
          (read-child [[children input]]
            (let [[child input] (read-node input)]
              [(conj children child) input]))]
    (first (read-node input))))

(def parse-puzzle (comp read-tree
                        ->numbers
                        ->words))

(def puzzle (read-puzzle "day8" parse-puzzle))

(defn solve-1 [input]
  (->> (tree-seq next rest input)
       (mapcat first)
       (apply +)))

(defn solve-2 [input]
  (letfn [(node-value [[metadata & children]]
            (if (empty? children)
              (apply + metadata)
              (->> metadata
                   (keep #(nth children (dec %) nil))
                   (map node-value)
                   (apply +))))]
    (node-value input)))

(comment
  (def sample (parse-puzzle "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"))
  (assert (= (solve-1 sample) 138))
  (solve-1 puzzle)

  (assert (= (solve-2 sample) 66))
  (solve-2 puzzle))
