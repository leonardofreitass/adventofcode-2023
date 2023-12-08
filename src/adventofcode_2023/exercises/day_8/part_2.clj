(ns adventofcode-2023.exercises.day-8.part-2 
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(defn make-map
  [inputs]
  (reduce
   #(let [[k l r] (re-seq #"[\w\d]+" %2)]
      (assoc %1 k {"L" l "R" r}))
   {}
   inputs))

(defn navigate
  [commands nav-map start]
  (loop [queue (cycle (str/split commands #""))
         curr start
         steps 0]
    (let [command (first queue)
          move (get-in nav-map [curr command])]
      (if (str/ends-with? move "Z")
        (inc steps)
        (recur (next queue) move (inc steps))))))

(defn run
  [[commands _ & inputs]]
  (let [nav-map (make-map inputs)
        starting-points (filter #(str/ends-with? % "A") (keys nav-map))
        paths (map (partial navigate commands nav-map) starting-points)] 
    (reduce math/lcm paths)))
