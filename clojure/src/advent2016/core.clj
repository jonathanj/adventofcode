(ns advent2016.core
  (:import [java.security MessageDigest])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^MessageDigest md5-digest (MessageDigest/getInstance "MD5"))

(defn ^String md5 [^String s]
  (format "%032x" (BigInteger. 1 (.digest (doto md5-digest .reset) (.getBytes s)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
