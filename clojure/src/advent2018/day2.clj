(ns advent2018.day2
  "https://adventofcode.com/2018/day/2"
  (:require [advent.core :refer [->lines]]
            [advent2018.core :refer [read-puzzle]]))

(def puzzle (read-puzzle "day2" ->lines))

(defn solve-1
  "Count the frequencies of letters, determine how many times frequencies of 2
  and 3 occur and multiply them together."
  [input]
  (let [freqs (->> input
                   (mapcat (comp set            ;; Collapse duplicates.
                                 vals           ;; Keep only the counts.
                                 frequencies))  ;; Count letter frequencies.
                   (frequencies))]
    (* (freqs 2) (freqs 3))))

(defn diff
  "Return a list of indices where two strings differ."
  [a b]
  (keep-indexed (fn [n [a b]] (when (not= a b) n))
                (map vector a b)))

(defn solve-2
  "Find the first string pair that has only a single difference and remove the
  difference."
  [input]
  (first (for [a     input
               b     input
               :let  [xs  (diff a b)
                      pos (first xs)]
               :when (= (count xs) 1)]
           (str (subs a 0 pos)
                (subs a (inc pos))))))

(comment
  (assert (= (solve-1 ["abcdef"
                       "bababc"
                       "abbcde"
                       "abcccd"
                       "aabcdd"
                       "abcdee"
                       "ababab"]) 12))
  (solve-1 puzzle)

  (assert (= (solve-2 ["abcde"
                       "fghij"
                       "klmno"
                       "pqrst"
                       "fguij"
                       "axcye"
                       "wvxyz"]) "fgij"))
  (solve-2 puzzle)
  )
