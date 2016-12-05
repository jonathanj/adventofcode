(ns advent2016.day5
  (:require [digest :as digest]
            [clojure.string :as s]))

(def example1 "abc")
(def puzzle "ffykfhsq")


(defn valid? [[key n]]
  (let [md5 (->> (format "%s%s" key n)
                 digest/md5)]
    (when (s/starts-with? md5 "00000")
      md5)))

(def solution-results (->> (map vector (repeat puzzle) (range))
                           (keep valid?)
                           #_(take 16)))

(def simple-result #(str (nth % 5)))
(def solution1 (->> solution-results
                    (map simple-result)
                    (take 8)
                    s/join))

(defn parse-position [c]
  (case c
    \0 0
    \1 1
    \2 2
    \3 3
    \4 4
    \5 5
    \6 6
    \7 7
    nil))

(defn complex-result [s]
  (if-let [n (parse-position (nth s 5))]
    [n (nth s 6)]))
(def solution2 (->> solution-results
                    (keep complex-result)
                    (reduce
                     (fn [result [n c]]
                       (println result n c)
                       (cond
                         (not-any? #(= \x %) result)
                         (do
                           (println "done")
                           (reduced result))
                         (= (nth result n) \x)
                         (do
                           (println "new one!")
                           (assoc result n c))
                         :else
                         (do
                           (println "old one!")
                           result)))
                     (vec (repeat 8 \x)))
                    (take 8)
                    s/join))

