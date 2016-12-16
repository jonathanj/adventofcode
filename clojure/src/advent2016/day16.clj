(ns advent2016.day16
  (:import [java.nio CharBuffer]))

(def example1 "10000")
(def puzzle "01110110101001000")

(defn ^CharBuffer alloc [^String s size]
  (doto (CharBuffer/allocate size)
    (.put s)))

(defn ^Character flip [^Character c]
  (case c
    \0 \1
    \1 \0))

(defn ^CharBuffer duplicate-backwards [^CharBuffer buf]
  (.put buf \0)
  (let [read                (.position buf)
        ^CharBuffer new-buf (.duplicate buf)
        count               (min read (inc (- (.capacity buf) read)))
        arr                 (char-array (dec count))]
    (.position new-buf (- (.position buf) count))
    (.get new-buf arr)
    (.put buf (char-array (reverse (map flip arr))))))

(defn ^CharBuffer fill-disk [^CharBuffer buf]
  (loop [buf buf]
    (if (zero? (.remaining buf))
      buf
      (recur (duplicate-backwards buf)))))

(defn checksum [^CharBuffer buf]
  (loop [^CharBuffer buf buf]
    (.position buf 0)
    (let [^CharBuffer result (CharBuffer/allocate (/ (.capacity buf) 2))]
      (dotimes [_ (.capacity result)]
        (let [a (.get buf)
              b (.get buf)]
          (.put result (if (= a b) \1 \0))))
      (if (odd? (.capacity result))
        (String. (.array result))
        (recur result)))))

(def solution1 (->> (alloc puzzle 272)
                    fill-disk
                    checksum))

(def solution2 (->> (alloc puzzle 35651584)
                    fill-disk
                    checksum))
