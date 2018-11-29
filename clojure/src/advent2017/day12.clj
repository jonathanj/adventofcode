(ns advent2017.day12
  "The puzzle input is an adjacency list. Part 1 is the number of nodes
  indirectly connected to node 0, which is a graph traversal from node 0. Part 2
  is the number of connected components in the graph: All distinct graph
  traversals from every node in the graph."
  (:require [loom.graph :refer [graph]]
            [loom.alg :refer [bf-traverse connected-components]]
            [advent.core :refer [->lines]]
            [advent2017.core :refer [read-puzzle]]))

(defn parse-adjacency [s]
  (let [[parent children] (clojure.string/split s #" <-> ")]
    {parent (clojure.string/split children #", ")}))

(defn lines->graph [lines]
  (apply graph (map parse-adjacency lines)))

(def sample-puzzle
  (lines->graph ["0 <-> 2"
                 "1 <-> 1"
                 "2 <-> 0, 3, 4"
                 "3 <-> 2, 4"
                 "4 <-> 2, 3, 6"
                 "5 <-> 6"
                 "6 <-> 4, 5"]))
(def puzzle (read-puzzle "day12.data" (comp lines->graph ->lines)))

(def solve-1 (comp count #(bf-traverse % "0")))
(def solve-2 (comp count connected-components))

(comment
  (assert (= (solve-1 sample-puzzle) 6))
  (solve-1 puzzle)

  (assert (= (solve-2 sample-puzzle) 2))
  (solve-2 puzzle))
