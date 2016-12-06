(ns advent2016.day2)

(def keypad1 [[5 5]
              (str "....."
                   ".123."
                   ".456."
                   ".789."
                   ".....")])

(def keypad2 [[7 7]
              (str "......."
                   "...1..."
                   "..234.."
                   ".56789."
                   "..ABC.."
                   "...D..."
                   ".......")])

(def example1 ["ULL" "RRDDD" "LURDL" "UUUUD"])
(def puzzle (vec (line-seq (clojure.java.io/reader "../day2.data"))))

(defn key-at [[[bx by] keypad] [x y]]
  (nth keypad (+ x (* y bx))))

(defn move [keypad [x y] instruction]
  (let [[nx ny] (case instruction
                  \U [x (dec y)]
                  \D [x (inc y)]
                  \L [(dec x) y]
                  \R [(inc x) y])]
    #_(println [x y] (key-at keypad [x y]) "--" instruction "-->" [nx ny] (key-at keypad [nx ny]))
    (if (= \. (key-at keypad [nx ny]))
      [x y]
      [nx ny])))

(defn solve [keypad start-pos instruction-list]
  (apply str (map (partial key-at keypad)
                  (rest (reductions
                         (partial reduce (partial move keypad))
                         start-pos
                         instruction-list)))))

(def solution1 (solve keypad1 [2 2] puzzle))
(def solution2 (solve keypad2 [1 3] puzzle))
