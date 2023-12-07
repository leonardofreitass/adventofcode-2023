(ns adventofcode-2023.exercises.day-7.part-2 
  (:require [clojure.string :as str]))

(defn card->value
  [card]
  (cond
    (= card "T") 10
    (= card "Q") 12
    (= card "K") 13
    (= card "A") 14
    (= card "J") 1
    :else (Integer/parseInt card)))

(defn non-nil
  [x n]
  (if (nil? x) n x))

(defn hand-type
  [hand]
  (let [freq (frequencies hand)
        sorted-freq (sort-by val > freq)
        highest-non-joker (non-nil (first (filter #(not= (key %) \J) sorted-freq)) (clojure.lang.MapEntry. \J 5))
        new-hand (str/replace hand #"J" (str (key highest-non-joker)))
        freq-vals (sort > (vals (frequencies new-hand)))]
    (cond
      (= (first freq-vals) 5) 7
      (= (first freq-vals) 4) 6
      (and (= (first freq-vals) 3) (= (second freq-vals) 2)) 5
      (= (first freq-vals) 3) 4
      (and (= (first freq-vals) 2) (= (second freq-vals) 2)) 3
      (= (first freq-vals) 2) 2
      :else 1)))

(defn sort-hands
  [[a] [b]]
  (let [a-type (hand-type a)
        b-type (hand-type b)]
    (if (= a-type b-type)
      (non-nil (some
                (fn [[a-card b-card]]
                  (let [a-value (card->value a-card)
                        b-value (card->value b-card)]
                    (if (= a-value b-value)
                      nil
                      (- a-value b-value))))
                (map vector (str/split a #"") (str/split b #"")))
               0)
      (- a-type b-type))))

(defn run
  [inputs]
  (reduce-kv
   (fn [acc k [_ v]]
     (+ acc (* (inc k) (Integer/parseInt v))))
   0
   (vec (sort sort-hands (map #(str/split % #" ") inputs)))))
