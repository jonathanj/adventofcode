(defproject advent2016 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.3.465"]
                 [digest "1.4.5"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [com.taoensso/tufte "1.1.0"]
                 [aysylu/loom "0.6.0"]
                 [org.flatland/useful "0.11.5"]
                 [org.clojure/data.finger-tree "0.0.2"]
                 [instaparse "1.4.8"]
                 ]
  :global-vars {*warn-on-reflection* true}
  :main ^:skip-aot advent2016.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
