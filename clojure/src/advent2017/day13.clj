(ns advent2017.day13
  ""
  (:require [advent.core :refer [->lines ->numbers]]
            [advent2017.core :refer [read-puzzle]]))

(defn parse-puzzle [lines]
  (->> lines
       (mapv (comp vec
                   ->numbers
                   #(clojure.string/split % #": ")))
       (into {})))

(def sample-puzzle
  (parse-puzzle ["0: 3"
                 "1: 2"
                 "4: 4"
                 "6: 4"]))
(def puzzle (read-puzzle "day13.data" (comp parse-puzzle ->lines)))

(defn solve
  ([t state]
   (solve -1 t state))
  ([c t state]
   (let [^long n (inc (apply max (keys state)))]
     (loop [       depth    0
            ^long  t        t
            caught          0
            ^long  severity nil]
       (if (or (== caught c) (== depth n))
         severity
         (let [^long size (get state depth)
               ^long x    (when size (rem t (* 2 (dec size))))
               caught?    (and x (== 0 x))
               ]
           (recur (inc depth)
                  (inc t)
                  (if caught?
                    (inc caught)
                    caught)
                  (if caught?
                    (+ (or severity 0) (* depth size))
                    severity))))))))

(defn solve-1 [input]
  (solve 0 input))

(defn solve-2 [input]
  (loop [t 0]
    (if-let [res (solve 1 t input)]
      (recur (inc t))
      t)))

(comment
  (assert (= (solve-1 sample-puzzle) 24))
  (solve-1 puzzle)

  (assert (= (solve-2 sample-puzzle) 10))
  (time (solve-2 puzzle)))
