(ns advent2016.day16
  (:require [flatland.useful.seq :refer [lazy-loop]]))

(def example1 "10000")
(def puzzle "01110110101001000")

(defn flip-bits [bits]
  (map not bits))

(defn inflate-once [required [n bits]]
  [(inc (* 2 n))
   (lazy-cat bits
             '(false)
             (flip-bits (reverse bits)))])

(defn inflate [target bits]
  (loop [[n bits :as input] bits]
    (let [required (- target n 1)]
      (if (< n target)
        (recur (inflate-once target input))
        [target (take target bits)]))))


(defn checksum-once [[_ bits]]
  (reduce (fn [[n bits] [a b]]
            [(inc n) (conj bits (= a b))])
          [0 []]
          (partition 2 bits)))

(defn checksum [bits]
  (loop [[n _ :as bits] (checksum-once bits)]
    (if (even? n)
      (recur (checksum-once bits))
      bits)))

(defn ->bits [s]
  [(count s) (map #(= % \1) s)])

(defn <-bits [[_ bits]]
  (apply str (map #(if % "1" "0") bits)))

(def solution1 (->> puzzle
                    ->bits
                    (inflate 272)
                    checksum
                    <-bits))

(def solution2 (->> puzzle
                    ->bits
                    (inflate 35651584)
                    checksum
                    <-bits))
