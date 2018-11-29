(ns advent2017.day9
  "A FSM that tracks group depths and non-discard garbage counts. See git
  history for the Instaparse grammar version."
  (:require [advent.core :refer [->lines]]
            [advent2017.core :refer [read-puzzle]]))

(def puzzle (read-puzzle "day9.data" (comp first ->lines)))

(defn solve [input]
  (reduce (fn [{:keys [mode depth] :as state} c]
            (case mode
              :group   (case c
                         \{ (-> state
                                (update :depth inc)
                                (update :depths conj (inc depth)))
                         \} (update state :depth dec)
                         \, state
                         \< (assoc state :mode :garbage))
              :garbage (case c
                         \! (assoc state :mode :discard)
                         \> (assoc state :mode :group)
                         (update state :garbage inc))
              :discard (assoc state :mode :garbage)))
          {:mode    :group
           :depth   0
           :depths  []
           :garbage 0}
          input))

(def solve-1 (comp (partial apply +) :depths solve))
(def solve-2 (comp :garbage solve))

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
