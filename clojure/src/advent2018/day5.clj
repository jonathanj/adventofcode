(ns advent2018.day5
  "https://adventofcode.com/2018/day/5"
  (:require [clojure.string :as str]
            [advent2018.core :refer [read-puzzle]]))

(def puzzle (read-puzzle "day5" str/trim))

(defn reacting-pair [^Character c]
  (let [pair (str (Character/toLowerCase c)
                  (Character/toUpperCase c))]
    (format "%s|%s" pair (clojure.string/join (reverse pair)))))

(def letters "abcdefghijklmnopqrstuvwxyz")

(defn reacts? [^Character a ^Character b]
  (and (not (nil? a))
       (not (nil? b))
       (not= a b)
       (= (Character/toLowerCase a)
          (Character/toLowerCase b))))

(defn solve-1
  "Reduce the input into a seq with consecutive reacting pairs removed."
  [[x & xs]]
  (count (reduce
          (fn [[a & as] b]
            (if (reacts? a b)
              as
              (conj as a b)))
          (list x) xs)))

(defn solve-2
  "Find the minimum solution to part 1 for inputs with each unit type removed
  from the input."
  [input]
  (letfn [(without-unit [^Character c]
            (remove (fn [^Character x] (or (= x (Character/toLowerCase c))
                                           (= x (Character/toUpperCase c))))
                    input))]
    (apply min (for [c     letters
                     :let  [input' (without-unit c)]
                     :when (not= input input')]
                 (solve-1 input')))))

(comment
  (assert (= (solve-1 "dabAcCaCBAcCcaDA") 10))
  (solve-1 puzzle)

  (assert (= (solve-2 "dabAcCaCBAcCcaDA") 4))
  (solve-2 puzzle))
