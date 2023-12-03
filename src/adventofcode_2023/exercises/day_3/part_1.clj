(ns adventofcode-2023.exercises.day-3.part-1)

(defn re-pos [re s]
  (loop [m (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

(defn all-symbol-pos
  [inputs]
  (apply concat (map-indexed #(map (partial conj [%1]) (keys (re-pos #"[^\d\.]" %2))) inputs)))

(defn quadrant
  [i j n]
  [[(dec i) (inc i)] [(dec j) (+ j (count n))]])

(defn quadrant-includes-symbol?
  [[[ia ib] [ja jb]] s]
  (some (fn [[i j]] (and (<= ia i ib) (<= ja j jb))) s))

(defn run
  [inputs]
  (let [all-symbols (all-symbol-pos inputs)]
    (reduce-kv
     (fn [acc i line]
       (reduce
        (fn [acc [j n]]
          (if (quadrant-includes-symbol? (quadrant i j n) all-symbols)
            (+ acc (Integer/parseInt n))
            acc))
        acc
        (re-pos #"[\d]+" line)))
     0
     (vec inputs))))
