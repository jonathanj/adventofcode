(ns advent2016.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def puzzle (line-seq (io/reader "../day3.data")))


(defn read-sides [line]
  (map #(Integer/parseInt %) (remove s/blank? (s/split line #" "))))

(defn valid-triangle? [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

(def solution1 (->> puzzle
                    (map read-sides)
                    (filter valid-triangle?)
                    count))

(def solution2 (->> puzzle
                    (map read-sides)
                    (apply mapcat vector)
                    (partition 3)
                    (filter valid-triangle?)
                    count))
