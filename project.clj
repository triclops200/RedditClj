(defproject redditsucker "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/data.json "0.2.4"]
                 [clj-http "0.7.8"]
                 [clj-time "0.6.0"]]
  :main ^:skip-aot redditsucker.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
