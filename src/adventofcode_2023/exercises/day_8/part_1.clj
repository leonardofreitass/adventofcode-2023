(ns adventofcode-2023.exercises.day-8.part-1 
  (:require [clojure.string :as str]))

(defn make-map
  [inputs]
  (reduce
   #(let [[k l r] (re-seq #"[A-Z]+" %2)]
      (assoc %1 k {"L" l "R" r}))
   {}
   inputs))

(defn run
  [[commands _ & inputs]]
  (let [nav-map (make-map inputs)]
    (loop [queue (cycle (str/split commands #""))
           curr "AAA"
           steps 0]
      (let [command (first queue)
            move (get-in nav-map [curr command])]
        (if (= "ZZZ" move)
          (inc steps)
          (recur (next queue) move (inc steps)))))))
