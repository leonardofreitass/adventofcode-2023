(ns adventofcode-2023.exercises.day-4.part-2 
  (:require [clojure.set :as set]))

(defn parse-input
  [inputs]
  (map #(let [[_ id w n] (re-find #"Card\s+(\d+): ([\d\s]+) \| ([\d\s]+)" %)
              win (re-seq #"\d+" w)
              numbers (re-seq #"\d+" n)]
          {:win (set win) :numbers (set numbers) :id (Integer/parseInt id)})
       inputs))

(defn run
  [inputs]
  (:cards (reduce
           (fn [{:keys [cards multiplier]} {:keys [win numbers id]}]
             (let [int (set/intersection win numbers)
                   next (inc id)
                   new-cards (get multiplier id 1)
                   queued-cards (range next (+ next (count int)))]
               {:cards (+ cards new-cards)
                :multiplier (reduce #(assoc %1 %2 (+ new-cards (get %1 %2 1))) multiplier queued-cards)}))
           {:cards 0 :multiplier {}}
           (parse-input inputs))))
