(ns advent2016.day6)

(def example1 ["eedadn" "drvtee" "eandsr" "raavrd" "atevrs" "tsrnev" "sdttsa"
               "rasrtv" "nssdts" "ntnada" "svetve" "tesnvt" "vntsnd" "vrdear"
               "dvrsen" "enarar"])
(def puzzle (vec (line-seq (clojure.java.io/reader "../day6.data"))))

(def solution1 (apply str (map #(->> (frequencies %)
                                     (sort-by (comp - val))
                                     ffirst)
                               (apply map str puzzle))))

(def solution2 (apply str (map #(->> (frequencies %)
                                     (sort-by val)
                                     ffirst)
                               (apply map str puzzle))))
