(ns advent2017.day25
  (:require [instaparse.core :as insta]
            [advent.core :refer [->lines ->int]]
            [advent2017.core :refer [read-puzzle]]))

(def puzzle-parser
  (insta/parser
   "blueprint      = begin <nl> steps instructions+ <nl>?
    <nl>           = '\\n'
    <ws>           = #'\\s+'
    <begin>        = <'Begin in state '> state <'.'>
    <steps>        = <'Perform a diagnostic checksum after '> number <' steps.'>
    <number>       = #'\\d+'
    <state>        = #'[A-Z]'
    instructions   = <nl> <nl> state_header <nl> rule <nl> rule
    <state_header> = <'In state '> state <':'>
    rule           = <ws> <'If the current value is '> number <':'> rule_inner
    <rule_inner>   = write <nl> move <nl> next_state
    <write>        = <ws> <'- Write the value '> number <'.'>
    <move>         = <ws> <'- Move one slot to the '> direction <'.'>
    <next_state>   = <ws> <'- Continue with state '> state <'.'>
    <direction>    = 'right' | 'left'"))

(defn rule->op [rule]
  (let [right? (= :right (:move rule))
        w      (:write rule)]
    (fn [[c l r]]
      [(:next rule)
       [(or (peek (if right? r l)) 0)
        (if right?
          (conj l w)
          (if (empty? l) (empty l) (pop l)))
        (if right?
          (if (empty? r) (empty r) (pop r))
          (conj r w))]])))

(defn parse [input]
  (->> (puzzle-parser input)
       (insta/transform {:rule         (fn [n write move next-state]
                                         {:rule  (->int n)
                                          :write (->int write)
                                          :move  (keyword move)
                                          :next  next-state})
                         :instructions (fn [state & rules]
                                         {state (mapv rule->op (sort-by :rule rules))})
                         :blueprint    (fn [begin steps & states]
                                         {:begin begin
                                          :steps (->int steps)
                                          :ops   (apply merge states)})})))

(def sample-puzzle
  (parse (clojure.string/join
          "\n"
          ["Begin in state A."
           "Perform a diagnostic checksum after 6 steps."
           ""
           "In state A:"
           "  If the current value is 0:"
           "    - Write the value 1."
           "    - Move one slot to the right."
           "    - Continue with state B."
           "  If the current value is 1:"
           "    - Write the value 0."
           "    - Move one slot to the left."
           "    - Continue with state B."
           ""
           "In state B:"
           "  If the current value is 0:"
           "    - Write the value 1."
           "    - Move one slot to the left."
           "    - Continue with state A."
           "  If the current value is 1:"
           "    - Write the value 1."
           "    - Move one slot to the right."
           "    - Continue with state A."])))
(def puzzle (read-puzzle "day25.data" parse))

(defn solve [{:keys [begin steps ops]}]
  (let [f       (fn [[state [c l r]]]
                  (let [f (nth (ops state) c)]
                    (f [c l r])))
        initial [0 clojure.lang.PersistentQueue/EMPTY (list)]
        [c l r] (->> (iterate f [begin initial])
                     (drop steps)
                     (first)
                     (second))]
    (+ c (apply + (concat l r)))))

(comment
  (assert (= (solve sample-puzzle) 3))
  (solve puzzle))
