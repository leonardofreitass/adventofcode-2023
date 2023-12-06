(ns adventofcode-2023.exercises.day-6.part-2 
  (:require [clojure.string :as str]))

(defn quadratic-function
  [t d]
  (Math/sqrt (- (Math/pow t 2) (* 4 d))))

(defn run
  [inputs]
  (let [t (bigint (str/join "" (re-seq #"[\d]+" (first inputs))))
        d (bigint (str/join "" (re-seq #"[\d]+" (second inputs))))]
    (dec (-
          (Math/ceil (/ (+ t (quadratic-function t d)) 2))
          (Math/floor (/ (- t (quadratic-function t d)) 2))))))
