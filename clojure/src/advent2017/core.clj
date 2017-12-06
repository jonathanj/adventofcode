(ns advent2017.core)

(defn read-puzzle
  ([name]
   (read-puzzle name identity))
  ([name xform]
   (-> (clojure.java.io/file (str "../2017/" name))
       (slurp)
       xform)))

(defn enumerate [xs]
  (map vector (range) xs))

(defn ->lines [s]
  (clojure.string/split s #"\n"))

(defn ->words [s]
  (clojure.string/split s #"\s+"))

(defn ->numbers [xs]
  (map #(Integer/parseInt %) xs))

(def words->numbers (comp ->numbers ->words))
(def lines->numbers (partial map words->numbers))

(defn manhattan-distance
  ([[x2 y2]]
   (manhattan-distance [0 0] [x2 y2]))
  ([[x1 y1] [x2 y2]]
   (+ (Math/abs (- x1 x2))
      (Math/abs (- y1 y2)))))
