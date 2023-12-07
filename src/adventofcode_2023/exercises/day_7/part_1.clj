(ns adventofcode-2023.exercises.day-7.part-1 
  (:require [clojure.string :as str]))

(defn card->value
  [card]
  (cond
    (= card "T") 10
    (= card "J") 11
    (= card "Q") 12
    (= card "K") 13
    (= card "A") 14
    :else (Integer/parseInt card)))

(defn non-nil
  [x]
  (if (nil? x) 0 x))

(defn hand-type
  [hand]
  (let [freq (sort > (vals (frequencies hand)))]
    (cond
      (= (first freq) 5) 7
      (= (first freq) 4) 6
      (and (= (first freq) 3) (= (second freq) 2)) 5
      (= (first freq) 3) 4
      (and (= (first freq) 2) (= (second freq) 2)) 3
      (= (first freq) 2) 2
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
       (map vector (str/split a #"") (str/split b #""))))
      (- a-type b-type))))

(defn run
  [inputs]
  (reduce-kv
    (fn [acc k [_ v]]
      (+ acc (* (inc k) (Integer/parseInt v))))
    0
    (vec (sort sort-hands (map #(str/split % #" ") inputs)))))
