(ns adamharbin.advent.2023.18.2
  (:require [clojure.string :as s]
            [clojure.set :as cset]
            [adamharbin.advent.common :refer [vector+]]))

;; Parse Input

(defn parse-line [input]
  "Parse line, including decode of hex values into length and direction"
  (let [[_ dir-str _ hex-str] (re-matches #"(.) (.*) \(#*(.*)\)" input)
        distance (+
                  (bit-shift-left (Integer/parseInt (str (nth hex-str 0)) 16) 16)
                  (bit-shift-left (Integer/parseInt (str (nth hex-str 1)) 16) 12)
                  (bit-shift-left (Integer/parseInt (str (nth hex-str 2)) 16) 8)
                  (bit-shift-left (Integer/parseInt (str (nth hex-str 3)) 16) 4)
                  (bit-shift-left (Integer/parseInt (str (nth hex-str 4)) 16) 0))
        dir (case (last hex-str) \0 :R \1 :D \2 :L \3 :U)]
    [distance dir]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))

;; Part 2

(defn find-pos [start length dir]
  "Offset given position by length in the given direction."
  (case dir
    :U (vector+ start [(- length) 0])
    :D (vector+ start [length 0])
    :R (vector+ start [0 length])
    :L (vector+ start [0 (- length)])))

(defn find-lines [input]
  "Convert the input path specification into a series of point pairs. These
   pairs comprise line segments defining the perimeter of the shape."
  (loop [xs input
         curr-pos [0 0]
         lines #{}]
    (if (seq xs)
      (let [[length dir] (first xs)
            next-pos (find-pos curr-pos length dir)]
        (recur (rest xs) next-pos (conj lines [curr-pos next-pos])))
      lines)))

(defn is-inside? [lines [row col :as interior-point]]
  "Determines if a given point lies inside or outside the shape. The algorithm is based on counting
   the number of times we cross the line defining the perimeter. If we have crossed an odd number of
   times, we must be in the interior of the shape."
  (let [vertical-lines (filter (fn [[[r c] [r' c']]] (= c c')) lines)
        intersecting (filter (fn [[[r c] [r' c']]] (<= (min r r') row (max r r'))) vertical-lines)
        col-values (map (fn [[[r c] [_ _]]] c) intersecting)
        vertical-lines-to-left (filter #(< % col) col-values)]
    (odd? (count vertical-lines-to-left))))

(defn compute-area [row-vertices col-vertices [row col]]
  "Compute the area of the rectangular region of the grid
   containing this point."
  (let [left (apply max (filter #(< % col) col-vertices))
        right (apply min (filter #(< col %) col-vertices))
        top (apply max (filter #(< % row) row-vertices))
        bottom (apply min (filter #(< row %) row-vertices))]
    (* (inc (- right left))
       (inc (- bottom top)))))

(defn find-perimeter-segments [row-vertices col-vertices [row col]]
  "Find the line segments comprising the rectangular region of
   the grid containing this point."
  (let [left (apply max (filter #(< % col) col-vertices))
        right (apply min (filter #(< col %) col-vertices))
        top (apply max (filter #(< % row) row-vertices))
        bottom (apply min (filter #(< row %) row-vertices))]
     [[[top left] [top right]]
      [[bottom left] [bottom right]]
      [[top left] [bottom left]]
      [[top right] [bottom right]]]))

(defn find-corner-points [row-vertices col-vertices [row col]]
  "Find the corner points of the rectangular region of the grid contained
   by this point."
  (let [left (apply max (filter #(< % col) col-vertices))
        right (apply min (filter #(< col %) col-vertices))
        top (apply max (filter #(< % row) row-vertices))
        bottom (apply min (filter #(< row %) row-vertices))]
     [[top left] [top right] [bottom left] [bottom right]]))

(defn compute-segment-length [[[r c] [r' c']]]
  (if (= r r')
    (inc (- c' c))
    (inc (- r' r))))

(defn solve [input]
  "We find the area of the shape by decomposing it into a grid of rectangular regions
   where the turning points of the path define the grid coordinates. Then the problem
   is reduced to determining whether each rectangle falls inside or outside the shape,
   along with some additional accounting for borders/corners shared by these rectanges
   which are double counted in the 'area' calculation."
  (let [lines (find-lines input)
        points (apply concat lines)
        row-vertices (sort (into #{} (map first points)))
        col-vertices (sort (into #{} (map second points)))]
    (loop [xs (for [r row-vertices c col-vertices] [(inc r) (inc c)])
           area 0
           perimeter-segments []
           corner-points []
           i 0]
      (if (seq xs)
        (let [interior-point (first xs)]
          (if (is-inside? lines interior-point)
            (recur
              (rest xs)
              (+ area (compute-area row-vertices col-vertices interior-point))
              (into perimeter-segments (find-perimeter-segments row-vertices col-vertices interior-point))
              (into corner-points (find-corner-points row-vertices col-vertices interior-point))
              (inc i))
            (recur (rest xs) area perimeter-segments corner-points (inc i))))
        (- area
           (->> (frequencies perimeter-segments)
                (filter #(= 2 (second %))) ; Indicates this line segment was already counted in 2 areas
                (map (comp compute-segment-length first))
                (apply +))
           (->> (frequencies corner-points)
                (filter #(= 4 (second %))) ; Indicates this corner was discounted twice above; re-add
                count
                -))))))

(solve parsed-input)
