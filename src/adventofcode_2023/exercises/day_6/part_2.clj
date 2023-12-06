(ns adventofcode-2023.exercises.day-6.part-2 
  (:require [clojure.string :as str]))

(defn travel
  [t p]
  (* p (- t p)))

(defn first-best
  [t d r]
  (some #(if (> (travel t %) d) % false) r))

(defn run
  [inputs]
  (let [t (bigint (str/join "" (re-seq #"[\d]+" (first inputs))))
        d (bigint (str/join "" (re-seq #"[\d]+" (second inputs))))]
    (inc (- (first-best t d (range t 0 -1)) (first-best t d (range t))))))
