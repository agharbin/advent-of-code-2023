(ns adamharbin.advent.2023.14
  (:require [clojure.string :as s]
            [adamharbin.advent.common :refer [vector+]]))

;; Input Parsing

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv vec)))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))
(def grid-rows (count parsed-input))
(def grid-cols (count (first parsed-input)))

;; Logic Part 1

(defn seek-north [grid [rock-row rock-col]]
  "Find resting position of rock rolling north (negative row direction)"
  (let [obstacles (for [r (range 0 rock-row)
                       :when (#{\O \#} (get-in grid [r rock-col]))]
                    [r rock-col])
        first-obstacle (last (sort-by first obstacles))]
    (if (some? first-obstacle)
      (vector+ first-obstacle [1 0])
      [0 rock-col])))

(defn seek-south [grid [rock-row rock-col]]
  "Find resting position of rock rolling south (positive row direction)"
  (let [obstacles (for [r (range (inc rock-row) grid-rows)
                       :when (#{\O \#} (get-in grid [r rock-col]))]
                    [r rock-col])
        first-obstacle (first (sort-by first obstacles))]
    (if (some? first-obstacle)
      (vector+ first-obstacle [-1 0])
      [(dec grid-rows) rock-col])))

(defn seek-east [grid [rock-row rock-col]]
  "Find resting position of rock rolling east (negative column direction)"
  (let [obstacles (for [c (range (inc rock-col) grid-cols)
                       :when (#{\O \#} (get-in grid [rock-row c]))]
                    [rock-row c])
        first-obstacle (first (sort-by second obstacles))]
    (if (some? first-obstacle)
      (vector+ first-obstacle [0 -1])
      [rock-row (dec grid-cols)])))

(defn seek-west [grid [rock-row rock-col]]
  "Find resting position of rock rolling west (positive column direction)"
  (let [obstacles (for [c (range 0 rock-col)
                       :when (#{\O \#} (get-in grid [rock-row c]))]
                    [rock-row c])
        first-obstacle (last (sort-by second obstacles))]
    (if (some? first-obstacle)
      (vector+ first-obstacle [0 1])
      [rock-row 0])))

(defn move-rock [seek-fn]
  "Given a seek function, return a function that will move a boulder on the grid in that direction"
  (fn [grid old-position]
    (let [new-position (seek-fn grid old-position)]
      (-> grid
          (update-in old-position (fn [x] \.))
          (update-in new-position (fn [x] \O))))))

;; We need to move the boulders in a particular order for each
;; direction so they don't get in each other's way.

(defn order-north [rocks] (sort-by first rocks))
(defn order-south [rocks] (reverse (sort-by first rocks)))
(defn order-east [rocks] (reverse (sort-by second rocks)))
(defn order-west [rocks] (sort-by second rocks))

(defn find-rocks [grid]
  (for [i (range grid-rows) j (range grid-cols)
        :when (= \O (get-in grid [i j]))]
    [i j]))

(defn move-all [order-fn move-fn grid]
  (let [rocks (find-rocks grid)]
    (reduce move-fn grid (order-fn rocks))))

(defn calculate-load [grid]
  (let [rocks (find-rocks grid)]
    (apply + (map (fn [[r c]] (- grid-rows r)) rocks))))

;; Solve Part 1

(->> parsed-input
     (move-all order-north (move-rock seek-north))
     calculate-load)

;; Logic Part 2

(defn spin-cycle [grid]
  (->> grid
       (move-all order-north (move-rock seek-north))
       (move-all order-west (move-rock seek-west))
       (move-all order-south (move-rock seek-south))
       (move-all order-east (move-rock seek-east))))

;; Solve Part 2

;; Used manual testing to determine that my input entered a cycle
;; of period 9 starting at 176 iterations. This means iteration
;; 1 billion will have the same value as iteration 181.

(->> parsed-input
     (iterate spin-cycle)
     (drop 181)
     first
     calculate-load)
