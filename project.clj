(defproject yo-pong "0.1.0-SNAPSHOT"
  :description "A simple (but playable) 2.5D Pong, using yaw-reactive"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [yaw-reactive "0.3.0-SNAPSHOT"]]
  :main yo-pong.core
  :repl-options {:init-ns yo-pong.core}
  :profiles { :uberjar {:aot :all} })


