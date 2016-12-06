(ns advent2016.day5
  (:import [java.security MessageDigest])
  (:require [clojure.string :as s]))

(def example1 "abc")
(def puzzle "ffykfhsq")

(defn md5 [s]
  (let [digest (.digest (MessageDigest/getInstance "MD5") (.getBytes s))]
    (apply format (clojure.string/join (repeat (count digest) "%02x")) digest)))

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

