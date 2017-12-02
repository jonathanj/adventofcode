(ns advent2016.day22
  (:require [clojure.math.combinatorics :refer [combinations]])
  )

(def puzzle (vec (line-seq (clojure.java.io/reader "../day22.data"))))
#_(def puzzle (vec (line-seq (clojure.java.io/reader "../day22_mithrandi.data"))))

(defn parse [s]
  (letfn [(parse-size [s]
            (Integer/parseInt (subs s 0 (dec (count s)))))]
    (-> (zipmap [:node :total :used :available :used-percentage]
                (remove clojure.string/blank? (clojure.string/split s #" ")))
        (update :total parse-size)
        (update :used parse-size)
        (update :available parse-size)
        )))

(defn viable-pairs [network]
  (for [a     network
        b     network
        :let  [{:keys [used]}      a
               {:keys [available]} b]
        :when (and (not= a b)
                   (> used 0)
                   (<= used available))]
    [a b]))


(def network (delay (->> (drop 2 puzzle)
                         (map parse))))
; Not 1767, too high.
; Not 879
; Not 896, too high.
; Not 895
; Not 872, too low.
; Not 678, too low.
(def solution1 (delay (->> @network
                           viable-pairs)))
