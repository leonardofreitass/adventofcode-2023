(ns adventofcode-2023.exercises.day-3.part-2)

(defn re-pos [re s]
  (loop [m (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

(defn all-star-pos
  [inputs]
  (reduce-kv
   (fn [acc i line]
     (reduce #(assoc %1 [i %2] []) acc (keys (re-pos #"[\*]" line))))
   {}
   (vec inputs)))

(defn quadrant
  [i j n]
  [[(dec i) (inc i)] [(dec j) (+ j (count n))]])

(defn quadrant-stars
  [[[ia ib] [ja jb]] s]
  (map first (filter (fn [[[i j]]] (and (<= ia i ib) (<= ja j jb))) s)))

(defn all-gears
  [inputs]
  (filter
   #(= 2 (count (second %)))
   (reduce-kv
    (fn [acc i line]
      (reduce
       (fn [acc [j n]]
         (let [near-stars (quadrant-stars (quadrant i j n) acc)]
           (if (empty? near-stars)
             acc
             (reduce #(update %1 %2 conj (Integer/parseInt n)) acc near-stars))))
       acc
       (re-pos #"[\d]+" line)))
    (all-star-pos inputs)
    (vec inputs))))

(defn run
  [inputs]
  (reduce
   #(+ %1 (reduce * %2))
   0
   (map second (all-gears inputs))))
