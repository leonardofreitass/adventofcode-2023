(ns adventofcode-2023.exercises.day-6.part-1)

(defn travel
  [t p]
  (* p (- t p)))

(defn run
  [inputs]
  (let [time (map #(Integer/parseInt %) (re-seq #"[\d]+" (first inputs)))
        distance (map #(Integer/parseInt %) (re-seq #"[\d]+" (second inputs)))]
    (reduce
     *
     (map
      (fn [t d]
        (count (filter (partial < d) (map (partial travel t) (range t)))))
      time
      distance))))
