(ns advent2016.day4
  (:require [clojure.string :as s]))

(def example1 "aaaaa-bbb-z-y-x-123[abxyz]")
(def puzzle (line-seq (clojure.java.io/reader "../day4.data")))


(defn split-parts [s]
  (let [[_ letters sector checksum]
        (first (re-seq #"([\p{Lower}-]+)-(\d+)\[(\w+)\]" s))]
    [letters (Integer/parseInt sector) checksum]))

(defn top-5 [letters]
  (->> (s/replace letters #"-" "")
       frequencies
       (sort-by (fn [[k v]] [(- v) k]))
       (take 5)
       (map first)
       (s/join "")))

(defn real-room? [[letters _ checksum :as room]]
  (= (top-5 letters) checksum))

(defn rotate-character [offset c]
  (if (Character/isLetter c)
    (let [v      (int c)
          base   (if (>= v (int \a))
                   (int \a)
                   (int \A))
          offset (mod offset 26)]
      (char (+ (mod (+ (- v base) offset) 26)
               base)))
    c))

(defn rotate [[letters count checksum]]
  [(apply str (map #(rotate-character count %) letters))
   count
   checksum])

(def solution1 (->> puzzle
                    (map split-parts)
                    (filter real-room?)
                    (map second)
                    (apply +)))

(def solution2 (->> puzzle
                    (map split-parts)
                    (filter real-room?)
                    (map rotate)
                    (filter #(s/includes? (first %) "north"))))
