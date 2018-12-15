(ns advent2016.core
  (:import [java.security MessageDigest])
  (:require [flatland.useful.seq :refer [lazy-loop]]
            [advent.core :as advent])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^MessageDigest md5-digest (MessageDigest/getInstance "MD5"))

(defn ^String md5 [^String s]
  (format "%032x" (BigInteger. 1 (.digest (doto md5-digest .reset) (.getBytes s)))))

(defn bfs [adjacent start]
  (lazy-loop [seen #{}
              xs   [start]
              ys   []]
             (let [x (first xs)]
               (cond
                 (and (empty? xs)
                      (empty? ys)) (list)
                 (empty? xs)       (lazy-recur seen (reverse ys) [])
                 (seen x)          (lazy-recur seen (rest xs) ys)
                 :else             (cons x (lazy-recur (conj seen x)
                                                       (rest xs)
                                                       (concat (adjacent x) ys)))))))

(def read-puzzle (advent/read-puzzle 2018))
