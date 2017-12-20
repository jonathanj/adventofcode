(ns advent2017.day20
  "Part 1 is the particle with the smallest acceleration magnitude. Part 2 is a
  total hack because I don't know how to determine that remaining particles
  cannot collide."
  (:require [advent2017.core :refer [read-puzzle
                                     ->lines
                                     ->csv
                                     ->int
                                     manhattan-distance]]))

(defn ->component [s]
  (let [[_ & v] (re-find #"< ?(-?\d+), ?(-?\d+), ?(-?\d+)>,?" s)]
    (mapv ->int v)))

(defn line->particle [idx line]
  (let [[_ p v a] (clojure.string/split line #"[pav]=")]
    (assoc (zipmap [:p :v :a] (map ->component [p v a]))
           :idx idx)))

(def lines->particles (partial map-indexed line->particle))

(def sample-puzzle (lines->particles
                    ["p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>"
                     "p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"]))
(def collision-puzzle (lines->particles
                    ["p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>"
                     "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>"
                     "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>"
                     "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"]))
(def puzzle (read-puzzle "day20.data" (comp lines->particles
                                            ->lines)))

(defn tick-particle [{:keys [p v a] :as particle}]
  (let [v' (mapv + v a)
        p' (mapv + p v')]
    (assoc particle :v v' :p p')))

(defn solve-1 [input]
  (->> input
       (apply min-key (comp manhattan-distance :a))
       :idx))

(defn solve-2 [input]
  (loop [particles input
         ;; XXX: It actually converges much sooner than this.
         ttl       (count input)]
    (if (zero? ttl)
      (count particles)
      (let [particles  (map tick-particle particles)
            collisions (->> particles
                            (map :p)
                            (frequencies)
                            (filter #(> (val %) 1))
                            (keys)
                            (set))]
        (recur (if (empty? collisions)
                 particles
                 (remove (comp collisions :p) particles))
               (dec ttl))))))

(comment
  (assert (= (solve-1 sample-puzzle) 0))
  (solve-1 puzzle)

  (assert (= (solve-2 collision-puzzle) 1))
  (solve-2 puzzle))
