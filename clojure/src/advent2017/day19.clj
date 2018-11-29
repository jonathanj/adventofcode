(ns advent2017.day19
  "A breadth-first path traversal, where we keep moving in the same direction
  until it's no longer a valid neighbour. Part 1 is all of the letter nodes we
  visit. Part 2 is the number of nodes we visit."
  (:require [loom.alg-generic :refer [bf-traverse bf-path]]
            [advent.core :refer [->lines]]
            [advent2017.core :refer [read-puzzle]]))

(defn parse-map [lines]
  (let [w (count (first lines))
        h (count lines)]
    (assert (every? #(= w (count %)) lines))
    [[(.indexOf ^String (first lines) "|") 0 [0 1]]
     [(vec (apply concat lines)) [w h]]]))

(def sample-puzzle
  ["     |          "
   "     |  +--+    "
   "     A  |  C    "
   " F---|----E|--+ "
   "     |  |  |  D "
   "     +B-+  +--+ "])
(def puzzle (read-puzzle "day19.data" ->lines))

(def path? #{\| \- \+})
(def letter? (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def traversable? (clojure.set/union path? letter?))

(defn valid-step? [[m [w h]] [x y [dx dy]]]
  (let [nx (+ x dx)
        ny (+ y dy)]
    (when (and (< -1 nx w)
               (< -1 ny h)
               (traversable? (m (+ nx (* ny w)))))
      [nx ny [dx dy]])))

(defn neighbours [mp [x y [dx dy] :as step]]
  ;; If the next step in the same direction is valid, take it.
  (if-let [new-pos (valid-step? mp step)]
    [new-pos]
    ;; Otherwise find neighbours in the orthogonral direction.
    (remove nil?
            (map (comp (partial valid-step? mp)
                       (partial conj [x y]))
                 [[   dy  (- dx)]
                  [(- dy)    dx ]]))))

(defn solve [[step [m [w _] :as mp]]]
  (bf-traverse (partial neighbours mp) step
               :f (fn [[x y _] _ _]
                    (m (+ x (* w y))))))

(def solve-1 (comp clojure.string/join
                   (partial filter letter?)
                   solve))
(def solve-2 (comp count solve))

(comment
  (assert (= (solve-1 (parse-map sample-puzzle)) "ABCDEF"))
  (solve-1 (parse-map puzzle))

  (assert (= (solve-2 (parse-map sample-puzzle)) 38))
  (solve-2 (parse-map puzzle)))
