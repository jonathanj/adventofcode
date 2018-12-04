(ns advent2018.day4
  "https://adventofcode.com/2018/day/4"
  (:require [clojure.string :as str]
            [advent.core :refer [->lines ->int]]
            [advent2018.core :refer [read-puzzle]]))

(defn parse-event [event]
  (cond
    (str/includes? event "begins") [:begins (->> event
                                                 (re-find #"Guard #(\d+) begins shift")
                                                 (second)
                                                 (->int))]
    (str/includes? event "asleep") [:asleep]
    (str/includes? event "wakes") [:wakes]))

(defn parse-log [s]
  (let [[_ Y M D h m event] (re-find #"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})] (.*)" s)]
    {:timestamp (mapv ->int [Y M D h m])
     :m         (->int m)
     :event     (parse-event event)}))

(def parse-puzzle (comp (partial sort-by first)
                        (partial map parse-log)
                        ->lines))

(def puzzle (read-puzzle "day4" parse-puzzle))

(defn update-max-minute [m]
  (assoc m :max-minute (apply max-key val (:times m))))

(defn solve
  "Process the sorted guard log, keeping state for the active guard and their
  total sleeping time, as well as tracking minute frequencies."
  [input]
  (->> input
       (reduce (fn [acc log]
                 (let [[event id] (:event log)
                       m          (:m log)
                       prev-m     (:prev-m acc)]
                   (case event
                     :begins (assoc acc :id id :prev-m nil)
                     :asleep (assoc acc :prev-m m)
                     :wakes  (update-in acc [:times (:id acc)]
                                        #(-> %
                                             ;; Total sleeping time.
                                             (update :total (fnil + 0) (- m prev-m))
                                             ;; Minute frequencies.
                                             (update :times
                                                     (partial merge-with +)
                                                     (frequencies (range prev-m m)))
                                             ;; Maximum minute frequency.
                                             (update-max-minute))))))
               {})
       :times
       (map (fn [[k v]] (assoc v :id k)))))


(defn solve-1
  [input]
  (let [res   (solve input)
        guard (apply max-key :total res)
        m     (first (apply max-key val (:times guard)))]
    (* (:id guard) m)))

(defn solve-2
  [input]
  (let [res   (solve input)
        guard (apply max-key (comp val :max-minute) res)
        m     (key (:max-minute guard))]
    (* (:id guard) m)))

(comment
  (def sample-1 (parse-puzzle (str/join "\n" ["[1518-11-01 00:00] Guard #10 begins shift"
                                              "[1518-11-01 00:05] falls asleep"
                                              "[1518-11-01 00:25] wakes up"
                                              "[1518-11-01 00:30] falls asleep"
                                              "[1518-11-01 00:55] wakes up"
                                              "[1518-11-01 23:58] Guard #99 begins shift"
                                              "[1518-11-02 00:40] falls asleep"
                                              "[1518-11-02 00:50] wakes up"
                                              "[1518-11-03 00:05] Guard #10 begins shift"
                                              "[1518-11-03 00:24] falls asleep"
                                              "[1518-11-03 00:29] wakes up"
                                              "[1518-11-04 00:02] Guard #99 begins shift"
                                              "[1518-11-04 00:36] falls asleep"
                                              "[1518-11-04 00:46] wakes up"
                                              "[1518-11-05 00:03] Guard #99 begins shift"
                                              "[1518-11-05 00:45] falls asleep"
                                              "[1518-11-05 00:55] wakes up"])))

  (assert (= (solve-1 sample-1) 240))
  (solve-1 puzzle)

  (assert (= (solve-2 sample-1) 4455))
  (solve-2 puzzle))
