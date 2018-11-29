(ns advent2017.day7
  "Part 1 finds the node with the highest cumulative weight, which is the
  bottom-most node. Part 2 starts at the bottom most node, calculating the
  cumulative weight for each one, if the weight of one child is not equal the
  process is repeated for that child until no more imbalances are found. The
  result is the corrected weight value for the problematic child."
  (:require [advent.core :refer [->lines]]
            [advent2017.core :refer [read-puzzle]]))

(defn line->node [s]
  (let [[x xs]       (clojure.string/split s #" -> " 2)
        children     (if xs (clojure.string/split xs #", ") [])
        [_ parent w] (re-find #"(.*?) \((\d+)\)" x)]
    [parent [(Integer/parseInt w) children]]))
(def lines->puzzle #(->> % (map line->node) (into {})))

(def puzzle (read-puzzle "day7.data" (comp lines->puzzle ->lines)))
(def sample-puzzle
  (lines->puzzle ["pbga (66)"
                  "xhth (57)"
                  "ebii (61)"
                  "havc (66)"
                  "ktlj (57)"
                  "fwft (72) -> ktlj, cntj, xhth"
                  "qoyq (66)"
                  "padx (45) -> pbga, havc, qoyq"
                  "tknk (41) -> ugml, padx, fwft"
                  "jptl (61)"
                  "ugml (68) -> gyxo, ebii, jptl"
                  "gyxo (61)"
                  "cntj (57)"]))

(defn weight [tree root]
  (loop [w        0
         [k & ks] [root]]
    (let [[w' xs] (tree k)
          w       (+ w' w)
          ks      (concat xs ks)]
      (if (empty? ks)
        w
        (recur w ks)))))

(defn weights [input nodes]
  (map (juxt (partial weight input) identity) nodes))

(defn solve-1 [input]
  (->> (keys input)
       (weights input)
       (apply max-key first)
       (second)))

(defn unbalanced [xs]
  (let [correct (set (for [[w a] xs
                           [v b] xs
                           :when (and (not= a b) (= w v))]
                       [w a]))
        [w a]   (first (clojure.set/difference (set xs) correct))]
    (when a
      [(- w (ffirst correct)) a])))

(defn solve-2 [input]
  (loop [[correction node] [0 (solve-1 input)]]
    (let [[w children] (input node)
          result       (unbalanced (weights input children))]
      (if-not result
        (- w correction)
        (recur result)))))

(comment
  (assert (= (solve-1 sample-puzzle) "tknk"))
  (solve-1 puzzle)

  (assert (= (solve-2 sample-puzzle) 60))
  (solve-2 puzzle))
