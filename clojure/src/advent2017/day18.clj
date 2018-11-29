(ns advent2017.day18
  ""
  (:require [clojure.core.async :as async]
            [advent.core :refer [->int ->lines number-string?]]
            [advent2017.core :refer [read-puzzle]]))

(def registers (zipmap (map str "abcdefghijklmnopqrstuvwxyz")
                       (repeat 0)))

(defn op-value [op]
  (cond
    (nil? op)           nil
    (number-string? op) (constantly (->int op))
    :else               #(% op)))

(defn binary-op
  ([op]
   (binary-op op :unnamed))
  ([op name]
   (fn [[a a'] [b b']]
     (fn [reg _]
       (-> reg
           (update a op (b' reg))
           (update-in [:counts name] (fnil inc 0))
           (update :ip inc))))))

(def instruction-set-1
  {"set" (fn [[a _] [_ b']]
           (fn [reg _]
             (-> reg
                 (assoc a (b' reg))
                 (update :ip inc))))
   "add" (binary-op +)
   "mul" (binary-op * "mul")
   "mod" (binary-op mod)
   "jgz" (fn [[_ a'] [_ b']]
           (fn [reg _]
             (update reg :ip + (if (pos? (a' reg)) (b' reg) 1))))
   "rcv" (fn [[_ a'] _]
           (fn [reg _]
             (if (zero? (a' reg))
               (update reg :ip inc)
               (assoc reg :ip -1))))
   "snd" (fn [[_ a'] _]
           (fn [reg _]
             (-> reg
                 (assoc :snd (a' reg))
                 (update :ip inc))))})

(def instruction-set-2
  (assoc instruction-set-1
         "snd" (fn [[_ a'] _]
                 (fn [reg [[_ me-waiting?] [snd-ch them-waiting?]]]
                   (async/put! snd-ch (a' reg))
                   (-> reg
                       (update :snd (fnil inc 0))
                       (update :ip inc))))
         "rcv" (fn [[a _] _]
                 (fn [reg [[rcv-ch me-waiting?] [snd-ch them-waiting?]]]
                   (async/go
                     (let [v (async/poll! rcv-ch)]
                       (reset! me-waiting? true)
                       (if (nil? v)
                         (do (when @them-waiting?
                               (async/put! snd-ch ::deadlock)
                               (async/put! rcv-ch ::deadlock))
                             reg)
                         (do (reset! me-waiting? false)
                             (if (= v ::deadlock)
                               (assoc reg :ip -1)
                               (-> reg
                                   (assoc a v)
                                   (update :ip inc)))))))))))

(defn compile-instruction [instruction-set s]
  (let [[inst a b] (clojure.string/split s #" ")
        a'         (op-value a)
        b'         (op-value b)]
    ((get instruction-set inst) [a a'] [b b'])))

(defn execute [registers chs program]
  (let [size    (count program)
        process (assoc registers
                       :ip  0
                       :counts {})]
    (loop [{:keys [ip] :as process} process]
      (if-not (< -1 ip size)
        process
        (let [res ((nth program ip) process chs)]
          (if (map? res)
            (recur res)
            (recur (async/<! res))))))))

(defn execute-async [pid chs program]
  (let [size    (count program)
        process (assoc registers
                       "p"  pid
                       :pid pid)]
    (async/go
      (execute process chs program))))

(def sample-puzzle ["set a 1"
                    "add a 2"
                    "mul a a"
                    "mod a 5"
                    "snd a"
                    "set a 0"
                    "rcv a"
                    "jgz a -1"
                    "set a 1"
                    "jgz a -2"])
(def deadlock-puzzle ["snd 1"
                      "snd 2"
                      "snd p"
                      "rcv a"
                      "rcv b"
                      "rcv c"
                      "rcv d"])
(def puzzle (read-puzzle "day18.data" ->lines))

(defn solve-1 [input]
  (->> input
       (mapv (partial compile-instruction instruction-set-1))
       (execute 0 nil)
       (async/<!!)
       :snd))

(defn solve-2 [input]
  (let [program   (mapv (partial compile-instruction instruction-set-2) input)
        rcv0-ch   (async/chan 1)
        rcv1-ch   (async/chan 1)
        waiting0? (atom false)
        waiting1? (atom false)
        pid0      (execute 0 [[rcv0-ch waiting0?] [rcv1-ch waiting1?]] program)
        pid1      (execute 1 [[rcv1-ch waiting1?] [rcv0-ch waiting0?]] program)]
    (:snd (async/<!! pid1))))

(comment
  (assert (= (solve-1 sample-puzzle) 4))
  (solve-1 puzzle)

  (assert (= (solve-2 deadlock-puzzle) 3))
  (solve-2 puzzle))
