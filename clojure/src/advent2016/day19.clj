(ns advent2016.day19
  (:require [taoensso.tufte :as tufte :refer [p profile]]))

(def example 5)
(def puzzle 3012210)

(defn init [n]
  (map vector (range) (repeat n 1)))

(defn steal [[thief a] [victim b]]
  #_(println "steal" "thief" thief a "victim" victim b)
  [thief (+ a b)])

(defn to-the-left [_] 2)
(defn across-the-circle [circle]
  (int (/ (count circle) 2)))

(defn solve [circle move]
  (loop [behind []
         ahead  circle]
    #_(println "behind" behind "ahead" ahead)
    (let [n (move (concat ahead behind))]
      (cond
        (and (empty? ahead)
             (= 1 (count behind))) (inc (ffirst behind))
        (empty? ahead)             (recur [] behind)
        (= 1 (count ahead))        (recur [] (concat ahead behind))
        :else                      (recur (conj behind (steal (first ahead)
                                                              (first (drop (dec n) (concat ahead behind)))))
                                          (next (concat (take n ahead)
                                                        (drop (inc n) ahead))))))))

(defn routes-clockwise [n]
  (let [circle (cycle (range n))]
    (into (array-map) (map vector (take n circle) (take n (next circle))))))

(defn routes-midpoint [n]
  (fn [index])
  
  )

(defn solve-map [routes]
  (loop [current 0
         routes  (transient routes)
         gifts   (transient (into (array-map) (init (count routes))))]
    (let [target        (get routes current)
          target-target (get routes target)]
      (if (= current target)
        current
        (recur target-target
               (p ::update-routes
                  (assoc! routes current target-target))
               (p ::update-gifts
                  (assoc! gifts current (+ (get gifts current)
                                           (get gifts target)))))))))

(def solution1 (delay (-> puzzle
                          route-x
                          steal
                          inc)))
