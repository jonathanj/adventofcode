(ns advent2017.day20
  "Part 1 is the particle with the smallest acceleration magnitude. Part 2 is
  simulating the particles and eliminating collisions until there are no more
  collisions; which is detected by ensuring that the remaining particles when
  sorted by magnitude are also sorted by velocity and acceleration."
  (:require [advent.core :refer [->lines
                                 ->int]]
            [advent2017.core :refer [read-puzzle]]))

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

(defn tick-particle [{:keys [^longs p ^longs v ^longs a] :as particle}]
  (let [[px py pz]           p
        [vx vy vz]           v
        [ax ay az]           a
        [vx' vy' vz' :as v'] [(+ vx ax) (+ vy ay) (+ vz az)]
        p'                   [(+ px vx') (+ py vy') (+ pz vz')]]
    (assoc particle
           :v v'
           :p p')))

(defn ^long metric [^longs [x y z]]
  (+ (* x x) (* y y) (* z z)))

(defn sorted-by? [f xs]
  (loop [[x & xs'] xs]
    (if-not xs'
      true
      (and (<= (f x) (f (first xs')))
           (recur xs')))))

(defn solve-1 [input]
  (->> input
       (apply min-key (comp metric :a))
       :idx))

(defn solve-2 [input]
  (loop [particles input]
    (let [ps (sort-by (comp metric :p) particles)]
      (if (and (sorted-by? (comp metric :v) ps)
               (sorted-by? (comp metric :a) ps))
        (count particles)
        (let [ps'        (map tick-particle ps)
              collisions (->> ps'
                              (map :p)
                              (frequencies)
                              (filter #(> (val %) 1))
                              (keys)
                              (set))]
          (recur (remove (comp collisions :p) ps')))))))

(comment
  (assert (= (solve-1 sample-puzzle) 0))
  (solve-1 puzzle)

  (assert (= (solve-2 collision-puzzle) 1))
  (solve-2 puzzle))
