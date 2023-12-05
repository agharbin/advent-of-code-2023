(ns adamharbin.advent.2023.4.1
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
    (if (empty? winning)
      0
      (int (math/pow 2 (dec (count winning)))))))

(defn solve [input]
  (->> input
    (map get-card-value)
    (apply +)))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))
(solve parsed-input)
