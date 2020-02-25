(ns yo-pong.core
  (:require [yo-pong.pong :refer [start-pong!]])
  (:gen-class))

(defn -main
  "The entry point of the game."
  [& args]
  (println "Starting pong ...")
  (start-pong!))
