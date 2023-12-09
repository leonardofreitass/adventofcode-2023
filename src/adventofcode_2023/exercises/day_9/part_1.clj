(ns adventofcode-2023.exercises.day-9.part-1)

(defn lazy-diff
  [coll]
  (map (fn [[a b]] (- a b)) (partition 2 1 coll)))

(defn parse-inputs
  [inputs]
  (map
   (fn [line]
     (reverse (map #(Integer/parseInt %) (re-seq #"[\-\d]+" line))))
   inputs))

(defn find-next
  [coll]
  (let [diffs (lazy-diff coll)]
    (if (every? zero? diffs) (first coll) (+ (first coll) (find-next diffs)))))

(defn run
  [inputs]
  (reduce + (map find-next (parse-inputs inputs))))
