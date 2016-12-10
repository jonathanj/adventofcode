(ns advent2016.day10
  (:require [clojure.string :as s]))

(def example1 ["value 5 goes to bot 2"
               "bot 2 gives low to bot 1 and high to bot 0"
               "value 3 goes to bot 1"
               "bot 1 gives low to output 1 and high to bot 0"
               "bot 0 gives low to output 2 and high to output 0"
               "value 2 goes to bot 2"])
(def puzzle (vec (line-seq (clojure.java.io/reader "../day10.data"))))


(defn parse-instruction [s]
  (let [tokens (s/split s #" ")]
    (condp = (first tokens)
      "value" {:src nil
               :value (Integer/parseInt (second tokens))
               :dst (keyword (str (last (butlast tokens)) (last tokens)))}
      "bot"   {:src (keyword (str (first tokens) (second tokens)))
               :low (keyword (str (nth tokens 5) (nth tokens 6)))
               :high (keyword (str (last (butlast tokens)) (last tokens)))})))


(defn instruction-graph [instructions]
  [(->> instructions
        (remove :src)
        (map #(cons nil ((juxt :dst :value) %))))
   (->> instructions
        (filter :src)
        vec
        (reduce-kv (fn [m _ v]
                     (when (contains? m (:src v))
                       (throw (ex-info "Too many instructions" v)))
                     (assoc m (:src v) (dissoc v :src)))
                   {}))])

(defn give [instructions state src dst value]
  (let [st    (update state dst #(conj (or % #{}) value))
        chips (st dst)
        ins   (instructions dst)]
    (if (and ins (= 2 (count chips)))
      (let [st2 (give instructions st dst (:low ins) (apply min chips))]
        (give instructions st2 dst (:high ins) (apply max chips)))
      st)))

(defn start [[init instructions]]
  (reduce (fn [st [_ dst value]]
            (give instructions st nil dst value))
          {}
          init))

(def solution1 (->> puzzle
                    (map parse-instruction)
                    (instruction-graph)
                    start
                    (some (fn [[k v]]
                            (when (= v #{61 17}) k)))))

(def solution2 (->> puzzle
                    (map parse-instruction)
                    (instruction-graph)
                    start
                    ((juxt :output0 :output1 :output2))
                    (apply concat)
                    (apply *)))
