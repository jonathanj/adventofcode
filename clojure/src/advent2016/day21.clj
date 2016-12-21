(ns advent2016.day21
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :refer [permutations]]))

(def example ["swap position 4 with position 0"
              "swap letter d with letter b"
              "reverse positions 0 through 4"
              "rotate left 1 step"
              "move position 1 to position 4"
              "move position 3 to position 0"
              "rotate based on position of letter b"
              "rotate based on position of letter d"])
(def puzzle (vec (line-seq (clojure.java.io/reader "../day21.data"))))

(defn swap-positions [x y s]
  (let [x'      (min x y)
        y'      (max x y)
        [l r]   (split-at y' s)
        b       (first r)
        [l2 r2] (split-at x' l)
        a       (first r2)]
    (str (apply str l2)
         b
         (apply str (rest r2))
         a
         (apply str (rest r)))))

(defn reverse-subs [x y s]
  (let [x'    (min x y)
        y'    (inc (max x y))
        s'    (subs s x' y')
        [l r] (split-at x' s)]
    (str (apply str l)
         (apply str (reverse s'))
         (apply str (drop (- y' x') r)))))

(defn rotate-left [n s]
  (let [n (mod n (count s))]
    (str (apply str (drop n s))
         (apply str (take n s)))))

(defn rotate-right [n s]
  (let [n (mod n (count s))]
    (str (apply str (take-last n s))
         (apply str (drop-last n s)))))

(defn move-left [x y s]
  (let [[l r]   (split-at x s)
        a       (first r)
        [l2 r2] (split-at y l)]
    (str (apply str l2)
         a
         (apply str r2)
         (apply str (rest r)))))

(defn move-right [x y s]
  (let [[l r]   (split-at (inc y) s)
        [l2 r2] (split-at x l)
        a       (first r2)]
    (str (apply str l2)
         (apply str (rest r2))
         a
         (apply str r))))

(defn position-swapper [x y]
  (partial swap-positions x y))

(defn letter-swapper [a b]
  (fn [s]
    (swap-positions (s/index-of s a) (s/index-of s b) s)))

(defn reverser [x y]
  (partial reverse-subs x y))

(defn left-rotator [n]
  (partial rotate-left n))

(defn right-rotator [n]
  (partial rotate-right n))

(defn letter-forward-rotator [a]
  (fn [s]
    (let [index     (s/index-of s a)
          n         (+ (inc index)
                       (if (>= index 4) 1 0))]
      (rotate-right n s))))

(defn letter-backward-rotator [a]
  (fn [s]
    (get @rainbow-table [s a])))

(defn mover [x y]
  (cond
    (= x y) identity
    (> x y) (partial move-left x y)
    (< x y) (partial move-right x y)))

(defn reverse-instruction [[ins x y]]
  (case ins
    :position-swapper       [:position-swapper y x]
    :letter-swapper         [:letter-swapper y x]
    :left-rotator           [:right-rotator x]
    :right-rotator          [:left-rotator x]
    :letter-forward-rotator [:letter-backward-rotator x]
    :reverser               [:reverser x y]
    :mover                  [:mover y x]))

(defn parse [line]
  (let [tokens (s/split line #" ")]
    (case (first tokens)
      "swap"    (case (second tokens)
                  "position" [:position-swapper
                              (Integer/parseInt (nth tokens 2))
                              (Integer/parseInt (nth tokens 5))]
                  "letter"   [:letter-swapper
                              (nth tokens 2)
                              (nth tokens 5)])
      "rotate"  (case (second tokens)
                  "left"  [:left-rotator (Integer/parseInt (nth tokens 2))]
                  "right" [:right-rotator (Integer/parseInt (nth tokens 2))]
                  "based" [:letter-forward-rotator (nth tokens 6)])
      "reverse" [:reverser
                 (Integer/parseInt (nth tokens 2))
                 (Integer/parseInt (nth tokens 4))]
      "move"    [:mover
                 (Integer/parseInt (nth tokens 2))
                 (Integer/parseInt (nth tokens 5))])))

(defn execute [s [ins & params]]
  ((apply ({:position-swapper        position-swapper
            :letter-swapper          letter-swapper
            :left-rotator            left-rotator
            :right-rotator           right-rotator
            :letter-forward-rotator  letter-forward-rotator
            :letter-backward-rotator letter-backward-rotator
            :reverser                reverser
            :mover                   mover} ins) params) s))

(defn letter-rotator-rainbow-table [s]
  (into {}
        (for [cs (permutations s)
              c  s]
          (let [cs (apply str cs)
                c  (str c)
                a  ((letter-forward-rotator c) cs)]
            [[a c] cs]))))

(def rainbow-table (delay (letter-rotator-rainbow-table "abcdefgh")))

(def solution1 (delay (->> puzzle
                           (map parse)
                           (reduce execute "abcdefgh"))))
(def solution2 (delay (->> puzzle
                           (map parse)
                           reverse
                           (map reverse-instruction)
                           (reduce execute "fbgdceah"))))
