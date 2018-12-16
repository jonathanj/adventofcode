(ns advent2018.day12
  (:require [clojure.string :as str]
            [advent.core :refer [->lines unlines]]
            [advent2018.core :refer [read-puzzle]]))

(def sample (unlines ["initial state: #..#.#..##......###...###"
                      ""
                      "...## => #"
                      "..#.. => #"
                      ".#... => #"
                      ".#.#. => #"
                      ".#.## => #"
                      ".##.. => #"
                      ".#### => #"
                      "#.#.# => #"
                      "#.### => #"
                      "##.#. => #"
                      "##.## => #"
                      "###.. => #"
                      "###.# => #"
                      "####. => #"]))

(defn translate [c]
  (if (= c \#) 1 0))

(defn parse-puzzle [input]
  (let [[initial' _ & patterns'] (->lines input)
        initial                  (mapv translate (drop 15 initial'))
        patterns                 (into {} (mapv (fn [s]
                                                  (let [[a b] (str/split s #" => ")]
                                                    [(mapv translate a) (translate (first b))]))
                                                patterns'))]
    {:state    initial
     :patterns patterns}))

(defn generation [input]
  (let [{:keys [state patterns]} input
        c                        (+ 6 (count state))
        state                    (concat [0 0 0] state (repeat 0))]
    (assoc input
           :state (reduce
                   (fn [acc [i c]]
                     (let [chunk (vec (take 5 (drop (- i 3) state)))]
                       (conj acc
                             (get patterns chunk 0))))
                   []
                   (map vector (range 3 c) state)))))

(def puzzle (read-puzzle "day12" parse-puzzle))

(defn count-pots [n {:keys [state]}]
  (apply + (map * (range (- n) (count state)) state)))

(defn solve-1 [input]
  (count-pots 20 (nth (iterate generation input) 20)))

(defn stable? [tail]
  (and (>= (count tail) 10)
       (= 1 (count (set tail)))))

(defn solve-2 [input]
  (reduce
   (fn [[acc prev tail] n]
     (let [acc    (generation acc)
           c      (count-pots n acc)
           delta  (- c prev)]
       (if (stable? tail)
         (reduced (+ c (* (- 50000000000N n) (first tail))))
         [acc c (take 10 (cons delta tail))])))
   [input 0 (list 0)]
   (range 1 1000)))

(comment
  (assert (= (solve-1 (parse-puzzle sample)) 325))
  (solve-1 puzzle)
  (solve-2 puzzle))
