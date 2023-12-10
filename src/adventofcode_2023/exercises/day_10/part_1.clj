(ns adventofcode-2023.exercises.day-10.part-1
  (:require [clojure.string :as str]))

(defn parse-input
  [inputs]
  (vec (map #(str/split % #"") inputs)))

(defn find-s
  [grid]
  (some
   (fn [cord] (if (= "S" (get-in grid cord)) cord nil))
   (for [i (range (count grid))
         j (range (count (first grid)))] [i j])))

(defn next-path
  [grid prev [i j]]
  (first (filter
          #(not= % prev)
          (case (get-in grid [i j])
            "|" [[(inc i) j] [(dec i) j]]
            "-" [[i (inc j)] [i (dec j)]]
            "7" [[(inc i) j] [i (dec j)]]
            "J" [[(dec i) j] [i (dec j)]]
            "F" [[(inc i) j] [i (inc j)]]
            "L" [[(dec i) j] [i (inc j)]]))))

(defn start-paths
  [grid [i j]]
  (filter
   seq
   (map
    (fn [[x y allowed]]
      (if (str/includes? allowed (get-in grid [x y] "."))
        [x y]
        nil))
    [[(inc i) j "|LJ"]
     [(dec i) j "|7F"]
     [i (inc j) "-7J"]
     [i (dec j) "-LF"]])))

(defn run
  [inputs]
  (let [grid (parse-input inputs)
        start (find-s grid)
        start-paths (start-paths grid start)]
    (loop [[prev-a prev-b] [start start]
           [next-a next-b] start-paths
           dist 0]
      (if (= next-a next-b)
        (inc dist)
        (recur [next-a next-b]
               [(next-path grid prev-a next-a) (next-path grid prev-b next-b)]
               (inc dist))))))
