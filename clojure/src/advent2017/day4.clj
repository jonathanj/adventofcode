(ns advent2017.day4
  (:require [advent2017.core :refer [->words]]))


(defn solve [xform lines]
  (let [counter (atom 0)]
    (doseq [line lines
            :let [words (map xform (->words line))]]
      (when (= (count words)
               (count (set words)))
        (swap! counter inc)))
    @counter))

(def solve-1 (partial solve identity))
(def solve-2 (partial solve frequencies))

(comment
  (assert (= (solve-1 ["aa bb cc dd ee"]) 1))
  (assert (= (solve-1 ["aa bb cc dd aa"]) 0))
  (assert (= (solve-1 ["aa bb cc dd aaa"]) 1))
  (solve-1 (line-seq (clojure.java.io/reader "../2017/day4.data")))

  (assert (= (solve-2 ["abcde fghij"]) 1))
  (assert (= (solve-2 ["abcde xyz ecdab"]) 0))
  (assert (= (solve-2 ["a ab abc abd abf abj"]) 1))
  (assert (= (solve-2 ["iiii oiii ooii oooi oooo"]) 1))
  (assert (= (solve-2 ["oiii ioii iioi iiio"]) 0))
  (solve-2 (line-seq (clojure.java.io/reader "../2017/day4.data"))))
