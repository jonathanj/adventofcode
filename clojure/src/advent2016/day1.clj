(ns advent2016.day1
  (:require [clojure.string :as s]))


(def example1 "R2, L3")
(def example2 "R2, R2, R2")
(def example3 "R5, L5, R5, R3")
(def puzzle (vec (line-seq (clojure.java.io/reader "../day1.data"))))


(defn walk-one
  "Travel one position in the given heading."
  [heading [x y]]
  (condp = heading
    :n [x (dec y)]
    :s [x (inc y)]
    :w [(dec x) y]
    :e [(inc x) y]))


(defn walk
  "Given a heading, an instruction and a starting position, determine all the
  intermediate positions, including the end position."
  [heading [dir count] pos]
  (let [heading (rotate heading dir)]
    [heading
     (->> (iterate #(walk-one heading %) pos)
          (take (inc count))
          (drop 1)
          vec)]))


(defn rotate
  "Given a heading and an instruction direction, determine the new heading."
  [heading dir]
  (case dir
    :L (case heading
         :n :w
         :w :s
         :s :e
         :e :n)
    :R (case heading
         :n :e
         :e :s
         :s :w
         :w :n)))


(defn travel
  "Follow the instructions from a starting position.

  The result is a vector of each intermediate position travelled, including the
  end position."
  [pos instructions]
  (->> instructions
       (reductions (fn [[heading steps] instruction]
                     (walk heading instruction (peek steps)))
                   [:n [pos]])
       (mapcat second)
       vec))


(defn distance
  "Calculate the taxicab distance between two points."
  [[sx sy] [ex ey]]
  (+ (Math/abs (- ex sx))
     (Math/abs (- ey sy))))


(defn translate-instruction
  "Translate a string instruction such as \"R2\" into `[:R 2]`."
  [instruction]
  [(keyword (subs instruction 0 1))
   (Integer/parseInt (subs instruction 1))])


(defn first-duplicate
  "Find the first duplicate value in a collection."
  [coll]
  (loop [seen     #{}
         [x & xs] coll]
    (cond
      (seen x)    x
      (empty? xs) nil
      :else       (recur (conj seen x) xs))))


(defn solve
  "Solve a puzzle given input instructions as a string."
  [s]
  (let [pos          [0 0]
        instructions (map translate-instruction (s/split s #", "))
        trail        (travel pos instructions)]
    (distance pos
              (or (first-duplicate trail)
                  (peek trail)))))
