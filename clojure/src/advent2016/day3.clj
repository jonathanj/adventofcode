(ns advent2016.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn read-sides [line]
  (map #(Integer/parseInt %) (remove s/blank? (s/split line #" "))))


(defn valid-triangle? [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

(def solution (->> "../day3.data"
                 io/reader
                 line-seq
                 (map read-sides)
                 (filter valid-triangle?)
                 count))
