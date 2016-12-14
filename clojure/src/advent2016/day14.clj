(ns advent2016.day14
  (:require [advent2016.core :refer [md5]]))

(def example "abc")
(def puzzle "cuanljph")

(def triple (comp second (partial re-find #"([0-9a-f])\1{2}")))
(defn quintuple? [^String digit ^String hash]
  (clojure.string/index-of hash (str digit digit digit digit digit)))

(defn key? [stream ^String hash]
  (if-let [digit (triple hash)]
    (some #(quintuple? digit %) (take 1000 stream))))

(defn hash-stream [f ^String salt]
  (map (fn [n] (list n (f (str salt n)))) (range)))

(defn key-stream [stream]
  (filter (fn [[n hash]] (key? (drop (inc n) stream) hash)) stream))

(defn stretched-md5 [^String hash]
  (nth (iterate md5 hash) 2017))

(def solution1-key-stream (->> puzzle
                               (hash-stream md5)
                               key-stream))
(def solution1 (take 64 solution1-key-stream))

(def solution2-key-stream (->> puzzle
                               (hash-stream stretched-md5)
                               key-stream))
(def solution2 (take 64 solution2-key-stream))
