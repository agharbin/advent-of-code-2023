(ns adamharbin.advent.2023.10.2
  (:require [clojure.string :as s]
            [clojure.set :as cset]
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

(defn find-loop-spaces [grid start next-move]
  (loop [pos next-move
         seen #{start}]
    (let [candidate (->> (neighbors grid pos) (filter (complement seen)) first)]
      (if (nil? candidate)
        (conj seen pos)
        (recur candidate (conj seen pos))))))

(defn clear-grid [grid loop-spaces]
  "removes extraneous 'debris' from the grid not part of the loop"
  (let [num-rows (count grid)
        num-cols (count (first grid))
        empty-grid (mapv
                   (fn [x]
                     (mapv
                       (fn [y] \.)
                       (range num-cols)))
                   (range num-rows))]
    (reduce #(update-in %1 %2 (fn [x] (get-in grid %2))) empty-grid loop-spaces)))

(defn open-vertically? [grid pos]
  (let [left-pipe (get-in grid (vector+ pos [0 -1]))
        right-pipe (get-in grid pos)]
    (not
      (or (contains? #{\- \F \L} left-pipe)
          (contains? #{\- \J \7} right-pipe)))))

(defn open-horizontally? [grid pos]
  (let [upper-pipe (get-in grid (vector+ pos [-1 0]))
        lower-pipe (get-in grid pos)]
    (not
      (or (contains? #{\| \F \7} upper-pipe)
          (contains? #{\| \J \L} lower-pipe)))))

(defn legal-move? [grid start-pos [offset end-pos]]
  "We are standing at the top left corner of the position being considered, so
  moving downward or right causes us to consider the contents of the tiles at the
  'current' grid position instead of the offset position."
  (case offset
    [1 0] (open-vertically? grid start-pos)
    [0 1] (open-horizontally? grid start-pos)
    [-1 0] (open-vertically? grid end-pos)
    [0 -1] (open-horizontally? grid end-pos)))

(defn in-grid? [grid [r c]]
  "We use <= for both ends of the interval since we have one more 'corner'
  than we have rows/columns"
  (let [row-count (count grid)
        col-count (count (first grid))]
    (and (<= 0 r row-count)
         (<= 0 c col-count))))

(defn adjacent-legal-corners [grid pos]
  (->> (for [offset [[1 0] [0 1] [-1 0] [0 -1]]]
         [offset (vector+ offset pos)])
       (filter #(in-grid? grid (second %)))
       (filter #(legal-move? grid pos %))
       (map second)))

(defn find-outside-spaces [grid start]
  "This function and its subroutines explore the graph from the perspective
  of the grid corners. This allows us to move 'through' two squares if the
  pipes are open in that direction. Our position is always the top left corner
  of the current grid square."
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY start)
         explored-corners #{}
         outside-spaces #{}]
    (if (seq q)
      (let [pos (peek q)]
        (if (explored-corners pos)
          (recur (pop q) explored-corners outside-spaces)
          (let [corners (adjacent-legal-corners grid pos)]
            (recur
              (into (pop q) corners)
              (conj explored-corners pos)
              (if (= \. (get-in grid pos))
                (conj outside-spaces pos)
                outside-spaces)))))
      outside-spaces)))

(defn solve [grid]
  (let [start-space (find-start grid)
        neighbors (for [offset [[0 1] [0 -1] [1 0] [-1 0]]]
                    (vector+ start-space offset))
        neighbors-in-loop (filter #(and
                                     (pipes-connect? grid start-space %)
                                     (is-in-loop? grid %))
                                  neighbors)
        neighbor-in-loop (first neighbors-in-loop)
        loop-spaces (find-loop-spaces grid start-space neighbor-in-loop)
        cleared-grid (clear-grid grid loop-spaces)
        outside-spaces (find-outside-spaces cleared-grid [0 0]) ; Assumes [0 0] is not in the loop
        num-tiles-not-included (count (cset/union loop-spaces outside-spaces))]
    (- (* (count grid) (count (first grid))) ; total spaces in grid
       num-tiles-not-included))) ; total spaces 'excluded' plus spaces occupied by loop

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))
(solve parsed-input)
