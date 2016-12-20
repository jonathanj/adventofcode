(ns advent2016.day11
  (:require [clojure.math.combinatorics :as comb]
            [loom.alg-generic :refer [bf-path bf-traverse]]))


(defn microchip [name] [false name])
(defn generator [name] [true name])
(defn item-type [[type _]] type)
(defn item-name [[_ name]] name)

(def example [0 [#{(microchip :h) (microchip :l)}
                 #{(generator :h)}
                 #{(generator :l)}
                 #{}]])

(def puzzle [0 [#{(microchip :promethium) (generator :promethium)}
                #{(generator :cobalt) (generator :curium) (generator :ruthenium) (generator :plutonium)}
                #{(microchip :cobalt) (microchip :curium) (microchip :ruthenium) (microchip :plutonium)}
                #{}]])

(def puzzle2 [0 [#{(microchip :promethium) (generator :promethium) (generator :elerium) (microchip :elerium) (generator :dilithium) (microchip :dilithium)}
                #{(generator :cobalt) (generator :curium) (generator :ruthenium) (generator :plutonium)}
                #{(microchip :cobalt) (microchip :curium) (microchip :ruthenium) (microchip :plutonium)}
                #{}]])

(defn current-floor [[elevator floors]]
  (nth floors elevator))

(defn items-legal? [items]
  (let [{chips      false
         generators true} (group-by item-type items)]
    (or (empty? chips)
        (empty? generators)
        (empty? (clojure.set/difference
                 (set (map item-name chips))
                 (set (map item-name generators)))))))

(defn legal-state? [[n floors :as state]]
  (cond
    (nil? state)          false
    (neg? n)              false
    (>= n (count floors)) false
    :else                 (every? items-legal? floors)))

(defn move [[n floors :as state] dir items]
  (let [next-n     (dir n)
        next-floor (get floors next-n)
        items      (set items)]
    (if (nil? next-floor)
      nil
      [next-n (-> floors
                  (update n #(clojure.set/difference % items))
                  (update next-n #(clojure.set/union % items)))])))

(defn neighbours [state]
  (let [current-floor (vec (current-floor state))]
    (for [items (concat (comb/combinations current-floor 1)
                        (comb/combinations current-floor 2))
          dir   [inc dec]
          :let  [st (move state dir items)]
          :when (legal-state? st)]
      st)))

(defn done? [st]
  (= st (done-state st)))

(defn done-state [[_ floors]]
  [3 [#{} #{} #{} (reduce clojure.set/union floors)]])

(def solution1 (delay (-> (bf-path neighbours puzzle (done-state puzzle))
                          count
                          dec)))

; XXX: Need to prune the decision tree quite dramatically to improve the runtime
; of this.
(def solution2 (delay (-> (some identity
                                (bf-traverse neighbours puzzle2
                                             :f (fn [n p d]
                                                  (when (done? n) p))))
                          (trace-path (done-state puzzle2))
                          count
                          dec)))
