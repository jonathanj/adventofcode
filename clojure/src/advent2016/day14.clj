(ns advent2016.day14
  (:require [advent2016.core :refer [md5]]))

(def example "abc")
(def puzzle "cuanljph")

(def triple (comp second (partial re-find #"([0-9a-f])\1{2}")))
(defn quintuple? [digit [_ hash]]
  (clojure.string/index-of hash (str digit digit digit digit digit)))

(defn key? [hashes hash]
  (if-let [digit (triple hash)]
    (some #(quintuple? digit %) (take 1000 hashes))))

(defn iterated-hash [iterations salt]
  (map (fn [n]
         (list n (nth (iterate md5 (str salt n)) iterations)))
       (range)))

(defn solve [salt iterations]
  (let [hashes (iterated-hash iterations salt)]
    (filter (fn [[n hash]] (key? (drop (inc n) hashes) hash))
            hashes)))

(def solution1 (take 64 (solve puzzle 1)))
(def solution2 (take 64 (solve puzzle 2017)))
