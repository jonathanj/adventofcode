(ns advent2017.core)

(defn enumerate [xs]
  (map vector (range) xs))

(defn lines->numbers [lines]
  (for [line lines]
    (map #(Integer/parseInt %) (clojure.string/split line #"\s+"))))
