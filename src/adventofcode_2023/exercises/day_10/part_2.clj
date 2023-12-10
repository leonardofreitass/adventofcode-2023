(ns adventofcode-2023.exercises.day-10.part-2  (:require [clojure.set :as set]
                                                         [clojure.string :as str]))

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
  [c prev [i j]]
  (first (filter
          #(not= (first %) prev)
          (case c
            "|" [[[(inc i) j] :s] [[(dec i) j] :n]]
            "-" [[[i (inc j)] :e] [[i (dec j)] :w]]
            "7" [[[(inc i) j] :rs] [[i (dec j)] :lw]]
            "J" [[[(dec i) j] :ln] [[i (dec j)] :rw]]
            "F" [[[(inc i) j] :ls] [[i (inc j)] :re]]
            "L" [[[(dec i) j] :rn] [[i (inc j)] :le]]))))

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

(defn run-loop
  [grid start start-paths]
  (loop [prev start
         curr (second start-paths)
         travelled #{start}
         dirs {:r 0 :l 0}
         left #{}
         right #{}]
    (if (= start curr)
      [travelled dirs left right]
      (let [c (get-in grid curr)
            [next-p dir] (next-path c prev curr)
            [i j] curr]
        (recur curr
               next-p
               (conj travelled curr)
               (case dir
                 :rn (update dirs :r inc)
                 :rs (update dirs :r inc)
                 :rw (update dirs :r inc)
                 :re (update dirs :r inc)
                 :ln (update dirs :l inc)
                 :ls (update dirs :l inc)
                 :le (update dirs :l inc)
                 :lw (update dirs :l inc)
                 dirs)
               (case dir
                 :e (conj left [(dec i) j])
                 :n (conj left [i (dec j)])
                 :w (conj left [(inc i) j])
                 :s (conj left [i (inc j)])
                 :ls (conj left [(inc i) (inc j)])
                 :ln (conj left [(dec i) (dec j)])
                 :le (conj left [(dec i) (inc j)])
                 :lw (conj left [(inc i) (dec j)])
                 :re (apply conj left [[(dec i) j] [i (dec j)]])
                 :rw (apply conj left [[(inc i) j] [i (inc j)]])
                 :rn (apply conj left [[(inc i) j] [i (dec j)]])
                 :rs (apply conj left [[(dec i) j] [i (inc j)]])
                 left)
               (case dir
                 :e (conj right [(inc i) j])
                 :n (conj right [i (inc j)])
                 :w (conj right [(dec i) j])
                 :s (conj right [i (dec j)])
                 :rs (conj right [(inc i) (dec j)])
                 :rn (conj right [(dec i) (inc j)])
                 :re (conj right [(inc i) (inc j)])
                 :rw (conj right [(dec i) (dec j)])
                 :lw (apply conj right [[(dec i) j] [i (inc j)]])
                 :le (apply conj right [[(inc i) j] [i (dec j)]])
                 :ls (apply conj right [[(dec i) j] [i (dec j)]])
                 :ln (apply conj right [[(inc i) j] [i (inc j)]])
                 right))))))

(defn spread-inner
  [travelled inner]
  (loop [queue (vec inner)
         all inner]
    (let [[i j] (first queue)
          ngbr #{[(inc i) j] [(dec i) j] [i (inc j)] [i (dec j)]}
          to-add (set/difference ngbr all travelled)
          new-queue (apply conj (next queue) (vec to-add))]
      (if (empty? new-queue)
        all
        (recur new-queue (set/union all to-add))))))

(defn run
  [inputs]
  (let [grid (parse-input inputs)
        start (find-s grid)
        start-paths (start-paths grid start)
        [travelled {:keys [r l]} _left _right] (run-loop grid start start-paths)
        left (set/difference _left travelled)
        right (set/difference _right travelled)
        inner (spread-inner travelled (if (> r l) right left))]
    (count inner)))
