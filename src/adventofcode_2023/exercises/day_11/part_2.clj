(ns adventofcode-2023.exercises.day-11.part-2 
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def expand-factor (dec 1000000))

(defn transpose
  [coll]
  (apply mapv vector coll))

(defn all-galaxies
  [grid]
  (for [i (range (count grid))
        j (range (count (first grid)))
        :when (= "#" (get-in grid [i j]))] [i j]))

(defn empty-spaces
  [grid]
  (for [k (range (count grid))
        :when (nil? (some (partial = "#") (grid k)))] k))

(defn print-grid
  [grid]
  (println (str/join "\n" (map str/join grid))))

(defn run
  [inputs]
  (let [grid (mapv #(vec (str/split % #"")) inputs)
        empty-lines (empty-spaces grid)
        empty-columns (empty-spaces (transpose grid))
        galaxies (all-galaxies grid)
        pairs (combo/combinations galaxies 2)]
    (reduce
     +
     (map
      (fn [[[ia ja] [ib jb]]]
        (let [mini (min ia ib)
              minj (min ja jb)
              maxi (max ia ib)
              maxj (max ja jb)
              expanded-lines (filter #(< mini % maxi) empty-lines)
              expanded-columns (filter #(< minj % maxj) empty-columns)]
          (+
           (- maxi mini)
           (- maxj minj)
           (* expand-factor (count expanded-lines))
           (* expand-factor (count expanded-columns)))))
      pairs))))
