(ns advent2017.day16
  "Part 1 is just applying the instructions to the string. Part 2 can be greatly
  accelerated by finding the size of the cycle and determining what the
  billionth value will be."
  (:require [advent.core :refer [->csv ->lines ->int]]
            [advent2017.core :refer [read-puzzle]]))

(defn rotate-right [^long n state]
  (let [x (- (count state) n)]
    (vec (concat (subvec state x)
                 (subvec state 0 x)))))

(defn swap-index [^long a ^long b state]
  (assoc state
         a (nth state b)
         b (nth state a)))

(defn swap-name [a b ^clojure.lang.PersistentVector state]
  (swap-index (.indexOf state a)
              (.indexOf state b)
              state))

(defn compile-move [s]
  (let [cmd   (first s)
        [a b] (clojure.string/split (subs s 1) #"/" 2)]
    (case cmd
      \s (partial rotate-right (->int a))
      \x (partial swap-index (->int a) (->int b))
      \p (partial swap-name (first a) (first b)))))

(def sample-puzzle (map compile-move ["s1" "x3/4" "pe/b"]))
(def puzzle (read-puzzle "day16.data" (comp (partial map compile-move)
                                            ->csv
                                            first
                                            ->lines)))

;; https://en.wikipedia.org/wiki/Cycle_detection#Brent's_algorithm
(defn brent [f x0]
  (let [xs  (iterate f x0)
        lam (loop [power       1
                   lam         1
                   tort        (first xs)
                   [hare & hs] (next xs)]
              (if (= tort hare)
                lam
                (if (== power lam)
                  (recur (* power 2) 1 hare hs)
                  (recur power (inc lam) tort hs))))]
    (loop [[tort & ts] xs
           [hare & hs] (drop lam xs)
           mu          0]
      (if (= tort hare)
        [lam mu]
        (recur ts hs (inc mu))))))

(defn solve-1
  ([input]
   (solve-1 input "abcdefghijklmnop"))
  ([input state]
   (apply str (reduce (fn [state f] (f state)) (vec state) input))))

(defn solve-2
  ([input]
   (solve-2 input "abcdefghijklmnop"))
  ([input state]
   (let [f        (partial (memoize solve-1) input)
         [lam mu] (brent f state)
         n        (rem (- 1000000000 mu) lam)]
     (first (drop n (iterate f state))))))

(comment
  (assert (= (solve-1 sample-puzzle "abcde") "baedc"))
  (solve-1 puzzle)

  (assert (= (solve-2 sample-puzzle "abcde") "abcde"))
  (solve-2 puzzle))
