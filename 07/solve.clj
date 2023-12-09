(ns adamharbin.advent.2023.7.1
  (:require [clojure.string :as s]
            [adamharbin.advent.common :refer [zip]]))

;; Input Parsing

(defn parse-line [input]
  (let [[hand bid] (s/split input #"\s+")]
    [hand (parse-long bid)]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(def card-value {\2 1 \3 2 \4 3 \5 4 \6 5 \7 6 \8 7 \9 8 \T 9 \J 10 \Q 11 \K 12 \A 13})

(defn hand-strength [hand]
  (let [[c1 c2 c3 c4 c5] (map card-value hand)
        card-value (+ c5 (* 13 c4) (* 13 13 c3) (* 13 13 13 c2) (* 13 13 13 13 c1))
        card-counts (-> hand frequencies vals sort)
        hand-value (case card-counts
                     [5]         7000000
                     [1 4]       6000000
                     [2 3]       5000000
                     [1 1 3]     4000000
                     [1 2 2]     3000000
                     [1 1 1 2]   2000000
                     [1 1 1 1 1] 1000000)]
    (+ card-value hand-value)))

(defn solve [input]
  (let [sorted-by-rank (sort-by (comp hand-strength first) input)
        ordered-bids (map second sorted-by-rank)
        with-multiplier (zip ordered-bids (range 1 (inc (count input))))]
    (->> with-multiplier
         (map #(apply * %))
         (apply +))))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))
(solve parsed-input)
