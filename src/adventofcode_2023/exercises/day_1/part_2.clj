(ns adventofcode-2023.exercises.day-1.part-2 
  (:require [clojure.string :as str]))

(def replacements {"one" "1"
                   "two" "2"
                   "three" "3"
                   "four" "4"
                   "five" "5"
                   "six" "6"
                   "seven" "7"
                   "eight" "8"
                   "nine" "9"})

(def regex (re-pattern (str "(?=(" (str/join "|" (conj (keys replacements) "\\d")) "))")))

(defn replace-number
  [n]
  (get replacements n n))

(defn run
  [inputs]
  (reduce
   +
   (map #(let [numbers (map second (re-seq regex %))]
           (Integer/parseInt (str
                              (replace-number (first numbers))
                              (replace-number (last numbers)))))
        inputs)))
