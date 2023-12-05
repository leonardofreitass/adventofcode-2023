(ns adventofcode-2023.exercises.day-5.part-2)

(defn parse-input
  [inputs]
  (map
   (fn [line] (filter not-empty (map #(map bigint (re-seq #"[\d]+" %)) line)))
   (remove (partial = '("")) (partition-by (partial = "") inputs))))

(defn map-ranges
  [[to from l] [xa xb]]
  (let [endb (+ from l)
        diff (- to from)
        subrangea [xa (min (max xa from) xb)]
        subrangeb (mapv (partial + diff) [(min (max xa from) xb) (max (min endb xb) xa)])
        subrangec [(max (min endb xb) xa) xb]
        left (filterv (partial apply not=) [subrangea subrangec])
        mapped (filterv (partial apply not=) [subrangeb])]
    [left mapped]))

(defn run
  [inputs]
  (let [[[_seeds] & reducers] (parse-input inputs)
        seed-ranges (map (fn [[s l]] [s (+ s l)]) (partition 2 _seeds))]
    (reduce min
            (map first
                 (reduce
                  (fn [acc ranges]
                    (apply
                     concat
                     (reduce
                      (fn [[acc mapped] range]
                        (let [mapped-ranges (map (partial map-ranges range) acc)]
                          [(apply concat (mapv first mapped-ranges))
                           (apply concat mapped (mapv second mapped-ranges))]))
                      [acc []]
                      ranges)))
                  seed-ranges
                  reducers)))))
