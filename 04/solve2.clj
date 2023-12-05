(ns adamharbin.advent.2023.4.2
  (:require [clojure.string :as s]
            [clojure.math :as math]))

;; Input Parsing

(def line-regex #"Card\s+(.*):\s+(.*)\s+\|\s+(.*)")

(defn parse-line [input]
  (let [[_ card-num-str winning-nums-str my-nums-str] (re-matches line-regex input)
        card-num (parse-long card-num-str)
        winning-nums (into #{} (map parse-long (s/split winning-nums-str #"\s+")))
        my-nums (map parse-long (s/split my-nums-str #"\s+"))]
    [card-num winning-nums my-nums]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(defn get-card-value [[card-num winning-nums my-nums]]
  (let [winning (filter winning-nums my-nums)]
    (count winning)))

(defn solve [input]
  (loop [cards input
         counts (into {} (for [[card-num _ _] input] [card-num 1]))]
    (if (seq cards)
      (let [next-card (first cards)
            card-num (first next-card)
            card-value (get-card-value next-card)
            cards-won (range (inc card-num) (+ card-value card-num 1))
            multiplier (counts card-num)
            next-counts (reduce
                          (fn [m k] (update m k (fn [x] (+ x multiplier))))
                          counts
                          cards-won)]
        (recur (rest cards) next-counts))
      (->> counts vals (apply +)))))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))
(solve parsed-input)
