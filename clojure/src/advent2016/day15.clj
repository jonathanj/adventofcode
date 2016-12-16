(ns advent2016.day15)

(def example1 (rest (clojure.string/split-lines "
Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1.")))
(def puzzle (vec (line-seq (clojure.java.io/reader "../day15.data"))))

(defn drop-capsule [positions state time]
  (map (fn [t p m] (mod (+ t p) m)) time state positions))

(defn parse-input [lines]
  (letfn [(parse-line [s]
            (map #(Integer/parseInt %)
                 (rest (re-find #"Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+)." s))))]
    (mapv parse-line lines)))

(defn split-input [slots]
  (apply map vector slots))

(defn solved? [[positions state] n]
  (when (zero? (apply + (drop-capsule positions state (iterate inc (inc n)))))
    n))

(def solution1 (some (partial solved? (->> puzzle
                                           parse-input
                                           split-input))
                     (range)))

(def solution2 (some (partial solved? (-> puzzle
                                          parse-input
                                          (conj '(11 0))
                                          split-input))
                     (range)))
