(ns advent2016.day1
  (:require [clojure.string :as s]))


(def example1 "R2, L3")
(def example2 "R2, R2, R2")
(def example3 "R5, L5, R5, R3")
(def puzzle "R4, R3, R5, L3, L5, R2, L2, R5, L2, R5, R5, R5, R1, R3, L2, L2, L1, R5, L3, R1, L2, R1, L3, L5, L1, R3, L4, R2, R4, L3, L1, R4, L4, R3, L5, L3, R188, R4, L1, R48, L5, R4, R71, R3, L2, R188, L3, R2, L3, R3, L5, L1, R1, L2, L4, L2, R5, L3, R3, R3, R4, L3, L4, R5, L4, L4, R3, R4, L4, R1, L3, L1, L1, R4, R1, L4, R1, L1, L3, R2, L2, R2, L1, R5, R3, R4, L5, R2, R5, L5, R1, R2, L1, L3, R3, R1, R3, L4, R4, L4, L1, R1, L2, L2, L4, R1, L3, R4, L2, R3, L1, L5, R4, R5, R2, R5, R1, R5, R1, R3, L3, L2, L2, L5, R2, L2, R5, R5, L2, R3, L5, R5, L2, R4, R2, L1, R3, L5, R3, R2, R5, L1, R3, L2, R2, R1")


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
