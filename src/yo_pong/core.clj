(ns yo-pong.core
  (:require [yo-pong.pong :refer [start-pong!]])
  (:gen-class))

(defn -main
  "The entry point of the game."
  [& args]
  (println "Starting pong ...")
  (start-pong!))


(defn the-score
    []
        [:group :test/scoredisplay
               {:pos [0 2.5 -6]
               :rot [90 0 0]
               :scale 1}
            [:item :test/score {:mesh {:filename "./ressources/score.obj"}
                :pos [0 0 0]
                :rot [0 0 0]
                :scale 1}]
            [:item :test/scoreleft {:mesh {:filename "./ressources/n0.obj"}
                                        :pos [2.5 0 0]
                                        :rot [0 0 0]
                                        :scale 1}]
            [:item :test/scoresep {:mesh {:filename "./ressources/colon.obj"}
                                        :pos [3 0 0]
                                        :rot [0 0 0]
                                        :scale 1}]
            [:item :test/scoreright {:mesh {:filename "./ressources/n0.obj"}
                                        :pos [3.5 0 0]
                                        :rot [0 0 0]
                                        :scale 1}]])