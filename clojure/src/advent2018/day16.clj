(ns advent2018.day16
  (:require [clojure.edn :refer [read-string]]
            [advent.core :refer [->lines ->words ->numbers]]
            [advent2018.core :refer [read-puzzle]]))


(defn read-instruction [s]
  (->numbers (->words s)))

(defn parse-part1 [input]
  (map (fn [[before inst after _]]
         {:before (read-string (subs before 8))
          :after  (read-string (subs after 8))
          :inst   (read-instruction inst)})
       input))

(defn parse-puzzle [input]
  (let [[part1 part2] (clojure.string/split input #"\n\n\n\n")
        part1 (partition-all 4 (->lines part1))]
    {:samples (parse-part1 part1)
     :program (map read-instruction (->lines part2))}))

(def puzzle (read-puzzle "day16" (comp parse-puzzle clojure.string/trim)))

(defn binary-reg-op [op]
  (fn binary-reg-op' [registers a b c]
    (assoc registers c (op (registers a) (registers b)))))

(defn binary-imm-op [op]
  (fn binary-imm-op' [registers a b c]
    (assoc registers c (op (registers a) b))))

(def operations
  {:addr (binary-reg-op +)
   :addi (binary-imm-op +)
   :mulr (binary-reg-op *)
   :muli (binary-imm-op *)
   :banr (binary-reg-op bit-and)
   :bani (binary-imm-op bit-and)
   :borr (binary-reg-op bit-or)
   :bori (binary-imm-op bit-or)
   :setr (fn [registers a _ c]
           (assoc registers c (registers a)))
   :seti (fn [registers a _ c]
           (assoc registers c a))
   :gtir (fn [registers a b c]
           (assoc registers c (if (> a (registers b)) 1 0)))
   :gtri (fn [registers a b c]
           (assoc registers c (if (> (registers a) b) 1 0)))
   :gtrr (fn [registers a b c]
           (assoc registers c (if (> (registers a) (registers b)) 1 0)))
   :eqir (fn [registers a b c]
           (assoc registers c (if (= a (registers b)) 1 0)))
   :eqri (fn [registers a b c]
           (assoc registers c (if (= (registers a) b) 1 0)))
   :eqrr (fn [registers a b c]
           (assoc registers c (if (= (registers a) (registers b)) 1 0)))
   })

(defn matching-ops [{:keys [before after inst]}]
  (let [[op-code & args] inst]
    [op-code (set (for [[op-name f] operations
                        :let        [res (apply f before args)]
                        :when       (= res after)]
                    op-name))]))

(defn solve-1 [input]
  (->> input
       :samples
       (map matching-ops)
       (filter #(>= (count (second %)) 3))
       (count)))

(defn single-item? [xs]
  (= 1 (count xs)))

(defn resolve-ops [possibilities ops]
  (let [known (set (vals ops))]
    (apply merge ops (for [[op-code
                            op-names] possibilities
                           :let       [names (clojure.set/difference op-names known)]
                           :when      (single-item? names)]
                       [op-code (first names)]))))

(defn solve-2 [input]
  (let [possibilities (map matching-ops (:samples input))
        ops           (nth (iterate (partial resolve-ops possibilities) {})
                           (count operations))]
    (get (reduce
          (fn [registers [op & args]]
            (apply (-> op ops operations) registers args))
          [0 0 0 0]
          (:program input))
         0)))


(comment
  (solve-1 puzzle)
  (solve-2 puzzle))
