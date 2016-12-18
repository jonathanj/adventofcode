(ns advent2016.day18)

(def example1 "..^^.")
(def example2 ".^^.^.^^^^")
(def puzzle (slurp "../day18.data"))

(defn trap? [[a _ c]]
  (and (not= a c)
       (or a c)))

(defn rows [previous]
  (lazy-seq
   (let [[[_ b] :as xs] (partition 3 1 (repeat false) previous)
         next-tiles     (cons b (map trap? xs))]
     (cons (count (remove identity previous)) (rows next-tiles)))))

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
