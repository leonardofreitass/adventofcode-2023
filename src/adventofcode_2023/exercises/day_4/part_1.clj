(ns adventofcode-2023.exercises.day-4.part-1 
  (:require [clojure.set :as set]))

(defn parse-input
  [inputs]
  (map #(let [[_ w n] (re-find #"Card\s+\d+: ([\d\s]+) \| ([\d\s]+)" %)
              win (re-seq #"\d+" w)
              numbers (re-seq #"\d+" n)] 
          {:win (set win) :numbers (set numbers)})
       inputs))

(defn run
  [inputs]
  (reduce
   (fn [acc {:keys [win numbers]}]
     (let [int (set/intersection win numbers)]
       (if (empty? int)
         acc
         (+ acc (Math/pow 2 (dec (count int)))))))
   0
   (parse-input inputs)))
