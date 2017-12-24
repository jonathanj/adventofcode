(ns advent2017.day24
  ""
  (:require [loom.alg-generic :as ag :refer [bf-traverse bf-path]]
            [advent2017.core :refer [read-puzzle ->lines ->int]]))

(defn lines->puzzle [lines]
  (set (map (fn [s] (mapv ->int (clojure.string/split s #"/"))) lines)))

(def sample-puzzle (lines->puzzle ["0/2"
                                   "2/2"
                                   "2/3"
                                   "3/4"
                                   "3/5"
                                   "0/1"
                                   "10/1"
                                   "9/10"]))
(def puzzle (read-puzzle "day24.data" (comp lines->puzzle ->lines)))

(defn neighbours [[^long m _ available]]
  (for [[^long a ^long b :as p] available
        :let                    [a? (== m a)]
        :when                   (or a? (== m b))]
    [(if a? b a) p (disj available p)]))

(defn solve [input]
  (let [start [0 [0 0] input]
        paths (-> (ag/bf-span neighbours start)
                  (ag/trace-paths start))]
    (map #(mapcat second %) paths)))

(defn solve-1 [input]
  (apply max (map #(apply + %) (solve input))))

(defn solve-2 [input]
  (let [paths (solve input)
        m     (count (apply max-key count paths))]
    (apply max (map #(apply + %) (filter #(= (count %) m) paths)))))

(comment
  (assert (= (solve-1 sample-puzzle) 31))
  (solve-1 puzzle)

  (assert (= (solve-2 sample-puzzle) 19))
  (solve-2 puzzle))
