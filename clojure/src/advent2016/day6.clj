(ns advent2016.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example1 ["eedadn" "drvtee" "eandsr" "raavrd" "atevrs" "tsrnev" "sdttsa"
               "rasrtv" "nssdts" "ntnada" "svetve" "tesnvt" "vntsnd" "vrdear"
               "dvrsen" "enarar"])
(def puzzle (vec (line-seq (io/reader "../day6.data"))))
(def transpose (partial apply map (comp (partial s/join) vector)))

(def solution1 (s/join (map #(->> (frequencies %)
                                  (sort-by (comp - val))
                                  ffirst)
                            (transpose puzzle))))

(def solution2 (s/join (map #(->> (frequencies %)
                                  (sort-by val)
                                  ffirst)
                            (transpose puzzle))))
