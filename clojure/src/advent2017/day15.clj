(ns advent2017.day15
  "")

(def sample-puzzle [65 8921])
(def puzzle [679 771])

(defn generator
  ([factor]
   (generator factor (constantly true)))
  ([^long factor pred]
   #(->> (iterate (fn [^long x] (rem (unchecked-multiply factor x) 2147483647)) %)
         (drop 1)
         (filter pred))))

(defn judge [n as bs]
  (->> (map vector (range n) as bs)
       (filter (fn [[_ ^long a ^long b]]
                 (== (bit-and a 0x0000ffff)
                     (bit-and b 0x0000ffff))))
       (count)))

(defn solve-1 [n a b]
  (judge n
         ((generator 16807) a)
         ((generator 48271) b)))

(defn solve-2 [n a b]
  (judge n
         ((generator 16807 (fn [^long x] (== 0 (rem x 4)))) a)
         ((generator 48271 (fn [^long x] (== 0 (rem x 8)))) b)))

(comment
  (assert (= (apply solve-1 40000000 sample-puzzle) 588))
  (apply solve-1 40000000 puzzle)

  (assert (= (apply solve-2 5000000 sample-puzzle) 309))
  (apply solve-2 5000000 puzzle))
