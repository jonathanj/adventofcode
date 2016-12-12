(ns advent2016.day12)

(def example1 ["cpy 41 a"
               "inc a"
               "inc a"
               "dec a"
               "jnz a 2"
               "dec a"])
(def puzzle (vec (line-seq (clojure.java.io/reader "../day12.data"))))

(defn register-name? [s]
  (not (every? #(Character/isDigit %) s)))

(defn parse-instruction [s]
  (let [[ins & params] (clojure.string/split s #" ")]
    (condp = ins
      "cpy" (let [[s d] params
                  srcfn (if (register-name? s)
                          (keyword s)
                          (constantly (Integer/parseInt s)))
                  dst   (keyword d)]
              (fn [[ip registers]]
                [(inc ip) (assoc registers dst (srcfn registers))]))
      "inc" (let [dst (keyword (first params))]
              (fn [[ip registers]]
                [(inc ip) (update registers dst inc)]))
      "dec" (let [dst (keyword (first params))]
              (fn [[ip registers]]
                [(inc ip) (update registers dst dec)]))
      "jnz" (let [[s d] params
                  srcfn (if (register-name? s)
                          (keyword s)
                          (constantly (Integer/parseInt s)))
                  dst   (Integer/parseInt d)]
              (fn [[ip registers]]
                [(if (zero? (srcfn registers))
                   (inc ip)
                   (+ ip dst))
                 registers]))
      (throw (ex-info "Unknown instruction" {:ins ins :params params})))))

(defn execute-all [initial-state instructions]
  (loop [[ip _ :as state] initial-state]
    (if (< ip (count instructions))
      (recur ((nth instructions ip) state))
      state)))

(def solution1 (->> puzzle
                    (mapv parse-instruction)
                    (execute-all [0 {:a 0 :b 0 :c 0 :d 0}])))

(def solution2 (->> puzzle
                    (mapv parse-instruction)
                    (execute-all [0 {:a 0 :b 0 :c 1 :d 0}])))
