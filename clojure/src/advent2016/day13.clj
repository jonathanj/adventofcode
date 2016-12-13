(ns advent2016.day13
  (:require [loom.alg-generic :refer [bf-path bf-traverse]]))

(def example 10)
(def puzzle 1364)

(defn cell [magic x y]
  (Integer/bitCount (+ (* x x) (* 3 x) (* 2 x y) (* y y) y magic)))

(defn neighbours [magic [x y]]
  (letfn [(valid? [[x y]]
            (and (>= x 0)
                 (>= y 0)
                 (even? (cell magic x y))))]
    (filter valid?
            [[(inc x) y]
             [(dec x) y]
             [x (inc y)]
             [x (dec y)]])))

(def solution1 (-> (partial neighbours puzzle)
                   (bf-path [1 1] [31 39])
                   count
                   dec))

(def solution2 (-> (partial neighbours puzzle)
                   (bf-traverse [1 1]
                                :when (fn [_ _ depth]
                                        (<= depth 50)))
                   count))
