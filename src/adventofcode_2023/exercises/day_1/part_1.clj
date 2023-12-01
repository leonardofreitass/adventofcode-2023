(ns adventofcode-2023.exercises.day-1.part-1)

(defn run
  [inputs]
  (reduce
   +
   (map #(let [numbers (re-seq #"\d" %)] 
           (Integer/parseInt(str
                             (first numbers)
                             (last numbers))))
        inputs)))
