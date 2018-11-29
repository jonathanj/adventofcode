(ns advent2017.day8
  "Compile each line into a function that takes the current registers and checks
  the condition and alters the registers. Part 1 is just a matter of reducing
  over the input and checking the result. Part 2 is a matter of tracking the
  all-time-high at each iteration."
  (:require [advent.core :refer [->lines]]
            [advent2017.core :refer [read-puzzle]]))

(def instructions
  {"inc" +
   "dec" -
   ">"   >
   "<"   <
   ">="  >=
   "<="  <=
   "=="  =
   "!="  not=})

(defn line->instruction [s]
  (let [[_ dst inst' off' src cmp-inst' x']
        (re-find #"^(\w+) (\w+) ([0-9-]+) if (\w+) ([^ ]+) ([0-9-]+)$" s)
        off      (Integer/parseInt off')
        x        (Integer/parseInt x')
        inst     (instructions inst')
        cmp-inst (instructions cmp-inst')]
    (fn [reg]
      (if (cmp-inst (reg src 0) x)
        (update reg dst (fnil inst 0) off)
        reg))))
(def lines->program (partial map line->instruction))

(defn solve [input]
  (reduce (fn [[reg ath] f]
            (let [res (f reg)]
              [res (apply max ath (vals res))]))
          [{} Double/NEGATIVE_INFINITY] input))

(def sample-puzzle
  (map line->instruction ["b inc 5 if a > 1"
                          "a inc 1 if b < 5"
                          "c dec -10 if a >= 1"
                          "c inc -20 if c == 10"]))

(def puzzle (read-puzzle "day8.data" (comp lines->program ->lines)))

(def solve-1 (comp (partial apply max) vals first solve))
(def solve-2 (comp second solve))

(comment
  (assert (= (solve-1 sample-puzzle) 1))
  (solve-1 puzzle)

  (assert (= (solve-2 sample-puzzle) 10))
  (solve-2 puzzle))
