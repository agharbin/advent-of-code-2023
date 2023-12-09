(ns adamharbin.advent.2023.7.2
  (:require [clojure.string :as s]
            [adamharbin.advent.common :refer [zip vector+]]))

;; Input Parsing

(defn parse-line [input]
  (let [[hand bid] (s/split input #"\s+")]
    [hand (parse-long bid)]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(def card-value {\J 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \Q 11 \K 12 \A 13})

(defn hand-strength [hand]
  (let [[c1 c2 c3 c4 c5] (map card-value hand)
        card-value (+ c5 (* 13 c4) (* 13 13 c3) (* 13 13 13 c2) (* 13 13 13 13 c1))
        num-jokers (get (-> hand frequencies) \J 0)
        card-counts (->> hand (filter (complement #{\J})) frequencies vals sort reverse)
        with-jokers (vector+ [num-jokers 0 0 0 0] (if (empty? card-counts) [0] card-counts))
        hand-value (case with-jokers
                     [5]         7000000
                     [4 1]       6000000
                     [3 2]       5000000
                     [3 1 1]     4000000
                     [2 2 1]     3000000
                     [2 1 1 1]   2000000
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
