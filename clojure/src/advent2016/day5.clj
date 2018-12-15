(ns advent2016.day5
  (:import [java.security MessageDigest])
  (:require [clojure.string :as s]
            [advent2016.core :refer [read-puzzle md5]]))

(def example1 "abc")
(def puzzle (read-puzzle "day5"))

(def solution-results (->> (map (comp md5 str) (repeat puzzle) (range))
                           (filter #(s/starts-with? % "00000"))))

(def solution1 (->> solution-results
                    (map #(subs % 5 6))
                    (take 8)
                    s/join))

(defn complex-result [s]
  (let [n (Integer/parseInt (subs s 5 6) 16)]
    (when (< n 8)
      [n (nth s 6)])))
(def solution2 (->> solution-results
                    (keep complex-result)
                    (reduce
                     (fn [result [n c]]
                       (cond
                         (not-any? nil? result) (reduced result)
                         (nil? (nth result n))  (assoc result n c)
                         :else                  result))
                     (vec (repeat 8 nil)))
                    s/join))

