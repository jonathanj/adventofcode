(ns advent2016.day7
  (:require [clojure.string :as s]
            [advent.core :refer [->lines]]
            [advent2016.core :refer [read-puzzle]]))

(def example1 ["abba[mnop]qrst"
               "abcd[bddb]xyyx"
               "aaaa[qwer]tyui"
               "ioxxoj[asdfgh]zxcvbn"])
(def example2 ["aba[bab]xyz"
               "xyx[xyx]xyx"
               "aaa[kek]eke"
               "zazbz[bzb]cdb"])
(def puzzle (read-puzzle "day7" ->lines))

(defn hypernet-seqs [s] (mapcat rest (re-seq #"\[([^]]*?)\]" s)))
(defn supernet-seqs [s] (s/split (s/replace s #"\[[^]]*?\]" "|") #"\|"))


(defn abba? [s]
  (re-seq #"(?:([a-z])(?!\1)([a-z])\2\1)+" s))
(defn abba [s]
  (if (every? nil? (map abba? (hypernet-seqs s)))
    (some identity (map abba? (supernet-seqs s)))))

(def solution1 (->> puzzle
                    (keep abba)
                    count))


(defn aba? [[a b c]]
  (and (= a c)
       (not= a b)))

(defn one-aba-bab? [hns sn]
  (->> (partition 3 1 sn)
       (filter aba?)
       (filter (fn [[a b]]
                 (some #(s/includes? % (str b a b)) hns)))))

(defn aba-bab? [s]
  (mapcat (partial one-aba-bab? (hypernet-seqs s))
          (supernet-seqs s)))

(def solution2
  (->> puzzle
       (map aba-bab?)
       (filter (comp not empty?))
       count))
