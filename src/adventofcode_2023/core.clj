(ns adventofcode-2023.core
  (:require [adventofcode-2023.exercises :as exercises])
  (:gen-class))

(defn -main
  "Runs an advent of code exercise"
  [& [exercise part]]
  (exercises/execute exercise part))
