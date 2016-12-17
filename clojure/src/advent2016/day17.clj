(ns advent2016.day17
  (:require [advent2016.core :refer [md5]]
            [loom.alg-generic :refer [bf-traverse]]))

(def broken "hijkl")
(def example1 "ihgpwlah")
(def example2 "kglvqrro")
(def example3 "ulqzkmiv")
(def puzzle "veumntbg")

(defn move [[x y] dir]
  (condp = dir
    :U [x (dec y)]
    :D [x (inc y)]
    :L [(dec x) y]
    :R [(inc x) y]))

(defn legal [pos [dir open?]]
  (when (and open? (not= pos [3 3]))
    (let [[x y] (move pos dir)]
      (when (and (>= 3 x 0) (>= 3 y 0))
        dir))))

(defn doors [passcode [path pos]]
  (->> (map name path)
       (apply str)
       (str passcode)
       md5
       (take 4)
       (map #{\b \c \d \e \f})
       (map vector [:U :D :L :R])
       (keep #(legal pos %))
       (map (juxt (partial conj path)
                  (partial move pos)))))

(defn vault [[path pos] _ _] (when (= pos [3 3]) path))

(def solution1 (-> (partial doors puzzle)
                   (bf-traverse [[] [0 0]] :f vault)
                   (->> (remove nil?)
                        first
                        (map name)
                        (apply str))))

(def solution2 (-> (partial doors puzzle)
                   (bf-traverse [[] [0 0]] :f vault)
                   (->> (remove nil?)
                        (map count)
                        (apply max))))
