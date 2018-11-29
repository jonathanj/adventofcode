(ns advent.core)

(defn read-puzzle [year]
  (fn
    ([name]
     (read-puzzle name identity))
    ([name xform]
     (fn [year]
       (-> (clojure.java.io/file (format "../%s/%s" year name))
           (slurp)
           xform)))))

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

(defn ->int [x]
  (Integer/parseInt (clojure.string/trim x)))

(defn ->numbers [xs]
  (map ->int xs))

(def lines->numbers (comp (partial map (comp ->numbers ->words))
                          ->lines))

(def csv->numbers (comp ->numbers ->csv))

(defn manhattan-distance
  ([a] (manhattan-distance (repeat 0) a))
  ([a b] (apply + (map (comp #(Math/abs ^long %) -) a b))))

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

(defn num->binary [^Integer n padding]
  (let [bs (Integer/toBinaryString n)
        p  (- (Integer/numberOfLeadingZeros n) 24)
        r  (apply str (repeat p "0"))]
    (if (== p 8)
      r
      (str r bs))))

(defn number-string? [s]
  (re-find #"-?\d+" s))