(ns adventofcode-2023.exercises.day-5.part-1)

(defn parse-input
  [inputs]
  (map
   (fn [line] (filter not-empty (map #(map bigint (re-seq #"[\d]+" %)) line)))
   (remove (partial = '("")) (partition-by (partial = "") inputs))))

(defn find-range
  [n ranges]
  (some (fn [[to from l]]
          (when (<= from n (+ from l)) [to from l]))
        ranges))

(defn seed->location
  [reducers seed]
  (reduce
   (fn [acc ranges]
     (let [range (find-range acc ranges)]
       (if range
         (+ acc (- (first range) (second range)))
         acc)))
   seed
   reducers))

(defn run
  [inputs]
  (let [[[seeds] & reducers] (parse-input inputs)]
    (reduce min (map (partial seed->location reducers) seeds))))
