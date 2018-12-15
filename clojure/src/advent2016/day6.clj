(ns advent2016.day6
  (:require [advent.core :refer [->lines]]
            [advent2016.core :refer [read-puzzle]]))

(def example1 ["eedadn" "drvtee" "eandsr" "raavrd" "atevrs" "tsrnev" "sdttsa"
               "rasrtv" "nssdts" "ntnada" "svetve" "tesnvt" "vntsnd" "vrdear"
               "dvrsen" "enarar"])
(def puzzle (read-puzzle "day6" ->lines))
#_(def puzzle (vec (line-seq (clojure.java.io/reader "../day6.data"))))

(def solution1 (apply str (map #(->> (frequencies %)
                                     (sort-by (comp - val))
                                     ffirst)
                               (apply map str puzzle))))

(def solution2 (apply str (map #(->> (frequencies %)
                                     (sort-by val)
                                     ffirst)
                               (apply map str puzzle))))
