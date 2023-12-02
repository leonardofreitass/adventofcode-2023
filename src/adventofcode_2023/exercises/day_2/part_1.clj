(ns adventofcode-2023.exercises.day-2.part-1 
  (:require [clojure.string :as str]))

(def max-color {
          "red" 12
          "green" 13
          "blue" 14
})

(defn is-possible
  [round]
  (every?
   (fn [dice]
     (let [[n color] (str/split dice #" ")]
       (<= (Integer/parseInt n) (max-color color))))
   (str/split round #", ")))

(defn run
  [inputs]
  (reduce
   (fn [acc line]
     (let [[_ n game] (re-find #"Game ([\d]+): (.*)" line)
           rounds (str/split game #"; ")]
       (if (every? is-possible rounds)
         (+ acc (Integer/parseInt n))
            acc)))
   0
   inputs))
