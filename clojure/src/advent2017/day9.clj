(ns advent2017.day9
  "Write an instaparse grammar to parse the input into groups of garbage,
  ignoring the discard garbage. Part 1 is the sum of the depths of the groups.
  Part 2 is the sum of the number of non-discard garbage."
  (:require [instaparse.core :as insta]
            [advent2017.core :refer [read-puzzle ->lines bfs-lazy]]))

(def puzzle (read-puzzle "day9.data" (comp first ->lines)))

(def parser
  (memoize
   (insta/parser
    "group           = <'{'> (group_inner <','>)* group_inner? <'}'>
     <group_inner>   = group | garbage
     garbage         = <'<'> garbage_inner* <'>'>
     <garbage_inner> = <garbage_discard> | garbage_keep
     garbage_discard = #'!.'
     <garbage_keep>  = !garbage_discard (#'[^>]')")))

(defn solve-1 [input]
  (apply + (bfs-lazy (parser input)
                     (fn [a _ _] (= a :group))
                     (fn [_ depth _] (inc depth)))))

(defn solve-2 [input]
  (apply + (bfs-lazy (parser input)
                     (fn [a _ _] (= a :garbage))
                     (fn [a _ b] (count b)))))

(comment
  (assert (= (solve-1 "{}") 1))
  (assert (= (solve-1 "{{{}}}") 6))
  (assert (= (solve-1 "{{},{}}") 5))
  (assert (= (solve-1 "{{{},{},{{}}}}") 16))
  (assert (= (solve-1 "{<a>,<a>,<a>,<a>}") 1))
  (solve-1 puzzle)

  (assert (= (solve-2 "{<>}") 0))
  (assert (= (solve-2 "{<random characters>}") 17))
  (assert (= (solve-2 "{<{!>}>}") 2))
  (assert (= (solve-2 "{<{o\"i!a,<{i<a>}") 10))
  (solve-2 puzzle))
