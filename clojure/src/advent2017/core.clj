(ns advent2017.core)

(defn enumerate [xs]
  (map vector (range) xs))

(defn ->words [s]
  (clojure.string/split s #"\s+"))

(defn lines->numbers [lines]
  (for [line lines]
    (map #(Integer/parseInt %) (->words line))))

(defn manhattan-distance
  ([[x2 y2]]
   (manhattan-distance [0 0] [x2 y2]))
  ([[x1 y1] [x2 y2]]
   (+ (Math/abs (- x1 x2))
      (Math/abs (- y1 y2)))))
