(ns adamharbin.advent.2023.24.1
  (:require [clojure.string :as s]
            [adamharbin.advent.common :refer [vector-]]))

;; Parse Input

(def re #"(-?\d+),\s+(-?\d+),\s+(-?\d+)\s+@\s+(-?\d+),\s+(-?\d+),\s+(-?\d+)")

(defn parse-line [input]
  (let [[_ px py pz vx vy vz] (re-matches re input)]
    [(mapv parse-long [px py pz])
     (mapv parse-long [vx vy vz])]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))

;; Part 1

(def lower-bound 200000000000000)
(def upper-bound 400000000000000)

(defn slope-and-y-int [[px py _] [vx vy _]]
  "Compute slope and y-intercept of line in x,y plane."
  (let [slope (/ vy vx)
        b (+ (* (- slope) px) py)]
    [slope b]))

(defn compute-intersection [[p1 v1] [p2 v2]]
  "Find the point at which 2 lines intersect, or nil if they don't."
  (let [[m1 b1] (slope-and-y-int p1 v1)
        [m2 b2] (slope-and-y-int p2 v2)]
    (if (zero? (- m1 m2))
      nil
      (let [x-int (/ (- b2 b1) (- m1 m2))
            y-int (+ b1 (* m1 x-int))]
        [(float x-int) (float y-int)]))))

(defn is-in-range? [[x-int y-int]]
  (and
    (<= lower-bound x-int upper-bound) 
    (<= lower-bound y-int upper-bound)))

(defn reaches-in-future? [[p v] point]
  "Determine whether the line meets the intersecting point in the future or past."
  (or
    (and
      (pos? (- (second point) (second p)))
      (pos? (second v)))
    (and
      (neg? (- (second point) (second p)))
      (neg? (second v)))))
  
(defn solve [input]
  "Computes the number of lines that intersect, ignoring the z dimention."
  (->>
    (for [line1 input line2 input :when (not= line1 line2)]
      (let [intersection (compute-intersection line1 line2)]
        (if (nil? intersection)
          false
          (and (is-in-range? intersection)
               (reaches-in-future? line1 intersection)
               (reaches-in-future? line2 intersection)))))
    (filter true?)
    count
    (* 1/2)))

(solve parsed-input)

;; Part 2 : No code, used math software to solve system of equations.
