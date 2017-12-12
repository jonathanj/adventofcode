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

(defn ->csv
  ([s] (->csv s true))
  ([s trim?] (map (if trim? clojure.string/trim identity)
                  (clojure.string/split s #","))))

(defn ->numbers [xs]
  (map #(Integer/parseInt (clojure.string/trim %)) xs))

(def lines->numbers (comp (partial map (comp ->numbers ->words))
                          ->lines))

(def csv->numbers (comp ->numbers ->csv))

(defn manhattan-distance
  ([[x2 y2]]
   (manhattan-distance [0 0] [x2 y2]))
  ([[x1 y1] [x2 y2]]
   (+ (Math/abs (- x1 x2))
      (Math/abs (- y1 y2)))))

(defn bfs-lazy [tree pred f]
  ((fn step [queue]
     (lazy-seq
      (when (seq queue)
        (let [[[node & children] depth] (peek queue)
              xs (step (into (pop queue)
                             (map vector children (repeat (inc depth)))))]
          (if (pred node depth children)
            (cons (f node depth children) xs)
            xs)))))
   (conj clojure.lang.PersistentQueue/EMPTY [tree 0])))
