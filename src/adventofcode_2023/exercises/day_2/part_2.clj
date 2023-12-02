(ns adventofcode-2023.exercises.day-2.part-2 
  (:require [clojure.string :as str]))

(defn set-cube-power
  [rounds]
  (reduce
   *
   (vals
    (reduce
     (fn [acc round]
       (reduce
        (fn [acc dice]
          (let [[n color] (str/split dice #" ")]
            (update acc color max (Integer/parseInt n))))
        acc
        (str/split round #", "))) 
       {"green" 0
        "red" 0
        "blue" 0}
     rounds))))

(defn run
  [inputs]
  (reduce
   (fn [acc line]
     (let [[_ game] (re-find #"Game [\d]+: (.*)" line)
           rounds (str/split game #"; ")]
       (+ acc (set-cube-power rounds))))
   0
   inputs))
