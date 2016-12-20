(ns advent2016.day20)

(def example ["5-8" "0-2" "4-7"])
(def example2 ["0-7" "5-9" "11-26" "18-25"])
(def puzzle (vec (line-seq (clojure.java.io/reader "../day20.data"))))

(defn split-range [s]
  (map bigint (clojure.string/split s #"-")))

(defn input [n data]
  (->> data
       (map split-range)
       (sort-by first)
       (partition 2 1 (repeat (list (inc n) (inc n))))))

(defn gap-start [[[a1 a2] [b1 b2 :as b]]]
  (when (and b1 (< (inc a2) b1))
    [(inc a2) b1]))

(def solution1 (delay (->> (input 4294967295 puzzle)
                           (map gap-start)
                           (some first))))

(def gap-counts (partial reduce
                         (fn [[results n] [[a1 a2] _]]
                           [(if (< (inc n) a1)
                              (conj results (- a1 n 1))
                              results)
                            (max n a2)])
                         [[] 0]))

(def solution2 (delay (->> (input 4294967295 puzzle)
                           gap-counts
                           first
                           (apply +))))
