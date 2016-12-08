(ns advent2016.day8
  (:import [java.nio CharBuffer]))

(defn new-screen [w h]
  (let [size (* w h)
        buf (CharBuffer/allocate size)]
    (doseq [i (range size)]
      (.put buf i \.))
    [[w h] buf]))

(defn rect-positions [stride x y w h]
  (mapcat (fn [n]
         (let [idx (+ x (* (+ n y) stride))]
           (range idx (+ idx w))))
       (range h)))

(defn rect [[[stride] :as screen] w h]
  (update screen 1
          (partial reduce #(.put %1 %2 \#))
          (rect-positions stride 0 0 w h)))

(defn rotate-one [xs]
  (subs (str (last xs) xs) 0 (count xs)))

(defn rotate [n xs]
  (last (take (inc n) (iterate rotate-one xs))))

(defn pixels [buf ps]
  (reduce (fn [s i] (str s (.get buf i))) "" ps))

(defn put-string [buf pxs s]
  (->> (map vector pxs s)
       (reduce (fn [buf [idx c]]
                 (.put buf idx c))
               buf)))

(defn rotate-pixels [screen n pxs]
  (update screen 1
          (fn [buf]
            (->> (pixels buf pxs)
                 (rotate n)
                 (put-string buf pxs)))))

(defn rotate-row [[[stride] :as screen] row n]
  (let [idx (* row stride)]
    (rotate-pixels screen n
                   (range idx (+ idx stride)))))

(defn rotate-col [[[stride sh] :as screen] col n]
  (rotate-pixels screen n
                 (range col (* sh stride) stride)))

(defn draw [[[w h] buf :as screen]]
  (doseq [y (range h)
          :let [idx (* y w)]]
    (println (.. buf
                 (subSequence idx (+ idx w))
                 toString)))
  screen)


(defn parse-dimension [s]
  (map #(Integer/parseInt %) (clojure.string/split s #"x")))

(defn parse-rotate [[idx _ n]]
  [(Integer/parseInt (apply str (nnext idx)))
   (Integer/parseInt n)])

(defn parse-instruction [s]
  (let [[cmd & params] (clojure.string/split s #" ")]
    (case cmd
      "rect" [rect (parse-dimension (first params))]
      "rotate" (case (first params)
                 "row" [rotate-row (parse-rotate (next params))]
                 "column" [rotate-col (parse-rotate (next params))]))))

(defn enact-instruction [screen [f args]]
  (if f
    (apply f screen args)
    screen))

(defn count-set-pixels [[_ buf]]
  (->> (.toString buf)
       (filter #(= \# %))
       count))

(def example1 ["rect 3x2"
               "rotate column x=1 by 1"
               "rotate row y=0 by 4"
               "rotate column x=1 by 1"])
(def puzzle (vec (line-seq (clojure.java.io/reader "../day8.data"))))

(def final-screen (->> puzzle
                    (map parse-instruction)
                    (reduce enact-instruction (new-screen 50 6))
                    draw))
(def solution1 (->> final-screen
                    count-set-pixels))
(def solution2 (->> final-screen
                    draw))
