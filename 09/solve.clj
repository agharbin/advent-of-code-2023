(ns adamharbin.advent.2023.9
  (:require [clojure.string :as s]))

;; Parse Input

(defn parse-line [input]
  (map parse-long (s/split input #"\s+")))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))

;; Logic

(defn solve-case [input]
  (if (every? zero? input)
    0
    (let [pairs (partition 2 1 input)
          diffs (map #(- (second %) (first %)) pairs)]
      (+ (last input) (solve-case diffs)))))

(defn solve [input]
  (apply + (map solve-case input)))

(solve parsed-input)

(defn solve-case-2 [input]
  (if (every? zero? input)
    0
    (let [pairs (partition 2 1 input)
          diffs (map #(- (second %) (first %)) pairs)]
      (- (first input) (solve-case-2 diffs)))))

(defn solve-2 [input]
  (apply + (map solve-case-2 input)))

(solve-2 parsed-input)
