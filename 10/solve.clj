(ns adamharbin.advent.2023.10.1
  (:require [clojure.string :as s]
            [adamharbin.advent.common :refer [vector+]]))

;; Input Parsing

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv vec)))

;; Logic

(def tiles {
  \| [[1 0] [-1 0]]
  \- [[0 1] [ 0 -1]]
  \L [[-1 0] [0 1]]
  \J [[-1 0] [0 -1]]
  \7 [[1 0] [0 -1]]
  \F [[1 0] [0 1]]
  \S []
  \. []})

(defn find-start [input]
  (let [row-count (count input)
        col-count (count (first input))
        positions (for [r (range row-count) c (range col-count)] [r c])]
    (loop [xs positions]
      (if (= \S (get-in input (first xs)))
        (first xs)
        (recur (rest xs))))))

(defn neighbors [grid pos]
  (for [offset (->> (get-in grid pos) tiles)]
    (vector+ offset pos)))

(defn pipes-connect? [grid p1 p2]
  (some #{p1} (neighbors grid p2)))

(defn is-in-loop? [grid start]
  (loop [pos start
         seen #{}]
    (let [possible-next-move (->> (neighbors grid pos) (filter #(not= pos %)) first)]
      (cond
        (nil? possible-next-move) false
        (nil? (get-in grid possible-next-move)) false
        (not (pipes-connect? grid pos possible-next-move)) false
        (seen pos) true
        :else (recur possible-next-move (conj seen pos))))))

(defn measure-distance [grid start next-move]
  (loop [pos next-move ; Start with one 'move' already made so we know we are in the loop
         seen #{start}
         moves 1]
    (let [candidate (->> (neighbors grid pos) (filter (complement seen)) first)]
      (if (nil? candidate)
        (inc moves)
        (recur candidate (conj seen pos) (inc moves))))))

(defn solve [grid]
  (let [start-space (find-start grid)
        neighbors (for [offset [[0 1] [0 -1] [1 0] [-1 0]]]
                    (vector+ start-space offset))
        neighbors-in-loop (filter #(and
                                     (pipes-connect? grid start-space %)
                                     (is-in-loop? grid %))
                                  neighbors)
        neighbor-in-loop (first neighbors-in-loop)]
    (/ (measure-distance grid start-space neighbor-in-loop)
       2)))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))
(solve parsed-input)
