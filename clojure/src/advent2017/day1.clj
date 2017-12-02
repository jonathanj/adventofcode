(ns advent2017.day1)

(def puzzle (slurp (clojure.java.io/file "../2017/day1.data")))

(defn char->int [c]
  (- (int c) (int \0)))

(def by-one (constantly 1))
(def by-half #(/ % 2))

(defn solve [input increment]
  (let [incr (increment input)]
    (apply + (for [[n c]  (enumerate input)
                   :when (= c (nth input (mod (+ n incr) (count input))))]
                (char->int c)))))

(comment
  (assert (= (solve "1122" by-one) 3))
  (assert (= (solve "1111" by-one) 4))
  (assert (= (solve "1234" by-one) 0))
  (assert (= (solve "91212129" by-one) 9))
  (solve puzzle by-one)

  (assert (= (solve "1212" by-half) 6))
  (assert (= (solve "1221" by-half) 0))
  (assert (= (solve "123425" by-half) 4))
  (assert (= (solve "123123" by-half) 12))
  (assert (= (solve "12131415" by-half) 4))
  (solve puzzle by-half))
