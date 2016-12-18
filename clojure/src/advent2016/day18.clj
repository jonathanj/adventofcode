(ns advent2016.day18)

(def example1 "..^^.")
(def example2 ".^^.^.^^^^")
(def puzzle (slurp "../day18.data"))

(defn trap? [xs]
  (condp = xs
    [true true false]  true
    [false true true]  true
    [true false false] true
    [false false true] true
    false))

(defn rows [previous]
  (lazy-seq
   (let [tmp        (partition 3 1 (repeat false) previous)
         next-tiles (map trap? (cons (cons false (butlast (first tmp))) tmp))]
     (cons (count (filter false? previous)) (rows next-tiles)))))

(defn parse [s]
  (map (partial = \^) s))

(def solution1 (->> puzzle
                    parse
                    rows
                    (take 40)
                    (apply +)))

(def solution2 (->> puzzle
                    parse
                    rows
                    (take 400000)
                    (apply +)))
