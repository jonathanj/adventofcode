(ns advent2017.day7
  (:require [advent2017.core :refer [read-puzzle ->lines]]))


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
  (map (juxt identity (partial weight input)) nodes))

(defn solve-1 [input]
  (->> (keys input)
       (weights input)
       (apply max-key second)
       (first)))

(defn odd-one [xs]
  (->> xs
       (sort-by second)
       (partition-by second)
       (filter #(= 1 (count %)))
       (first)
       (ffirst)))

(defn solve-2 [input]
  (loop [root         (solve-1 input)
         prev-weights []]
    (let [[w children] (input root)
          weights      (weights input children)
          new-root     (odd-one weights)
          ]
      (if (nil? new-root)
        (+ w (apply - (sort (set (map second prev-weights)))))
        (recur new-root weights)))))

(comment
  (assert (= (solve-1 sample-puzzle) "tknk"))
  (solve-1 puzzle)

  (assert (= (solve-2 sample-puzzle) 60))
  (solve-2 puzzle))
