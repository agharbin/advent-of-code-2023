(ns adamharbin.advent.2023.17.2
  (:require [clojure.string :as s]
            [clojure.data.priority-map :refer [priority-map]]))

;; Parse Input

(defn parse-line [input]
  (mapv parse-long (s/split input #"")))

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv parse-line)))

(def raw-input (slurp "input.dat"))
(def grid (parse-input raw-input))
(def grid-rows (count grid))
(def grid-cols (count (first grid)))
(def all-positions (for [r (range grid-rows) c (range grid-cols) d [:left :right :up :down]] [r c d]))

;; Logic

(def min-dist 4)
(def max-dist 10)

(defn left-moves [[r c d]]
  (filter #(some? (get-in grid (take 2 %)))
          (for [i (range min-dist (inc max-dist))] [r (- c i) :right])))

(defn right-moves [[r c d]]
  (filter #(some? (get-in grid (take 2 %)))
          (for [i (range min-dist (inc max-dist))] [r (+ c i) :right])))

(defn up-moves [[r c d]]
  (filter #(some? (get-in grid (take 2 %)))
          (for [i (range min-dist (inc max-dist))] [(- r i) c :down])))

(defn down-moves [[r c d]]
  (filter #(some? (get-in grid (take 2 %)))
          (for [i (range min-dist (inc max-dist))] [(+ r i) c :up])))

(defn find-legal-moves [[r c dir :as curr-pos]]
  "Find all moves we are permitted to make from this position.
   For the starting node [0 0], we can move left or down. For all other nodes, we can move either
   left or right depending on the direction we came from last."
  (cond
    (nil? dir) (concat (down-moves curr-pos) (right-moves curr-pos))
    (or (= :left dir) (= :right dir)) (concat (down-moves curr-pos) (up-moves curr-pos))
    :else (concat (left-moves curr-pos) (right-moves curr-pos))))

(defn compute-cost [[r c _] [r' c' _]]
  "Compute the cost of a move.
   We allow traversal of multiple nodes at once, so we need to sum the cost of all traversed nodes."
  (let [low-r (min r r')
        high-r (max r r')
        low-c (min c c')
        high-c (max c c')]
    (-
      (if (= r r')
        (apply + (for [x (range low-c (inc high-c))] (get-in grid [r x])))
        (apply + (for [x (range low-r (inc high-r))] (get-in grid [x c]))))
      (get-in grid [r c]))))

(def start-pos [0 0 nil])
; The goal node can be reached from 2 directions. Both must be tried to get the correct answer.
(def goal [(dec grid-rows) (dec grid-cols) :up])

(defn find-goal []
  "An implementation of Dijkstra's algorithm for solving this problem.
   Standard Dijkstra's algorithm does not work, due to movement restrictions. To work around this,
   we treat a node aproached from 2 different directions as separate nodes. A position is represented as
   a 3-tuple [row col approach-direction]"
  (loop [q (priority-map start-pos 0)
         distances (assoc (into {} (for [pos all-positions] [pos Integer/MAX_VALUE])) start-pos 0)
         visited #{}]
    (if (or (not (seq q)) (visited goal))
      (distances goal)
      (let [curr-pos (first (peek q))
            curr-cost (distances curr-pos)
            legal-moves (find-legal-moves curr-pos)
            better-legal-moves (filter
                                 #(and (not (visited %))
                                       (< (+ curr-cost (compute-cost curr-pos %)) (distances %)))
                                 legal-moves)]
        (recur
          (into (pop q) (for [pos better-legal-moves] [pos (+ curr-cost (compute-cost curr-pos pos))]))
          (reduce
            (fn [m pos] (assoc m pos (+ curr-cost (compute-cost curr-pos pos))))
            distances
            better-legal-moves)
          (conj visited curr-pos))))))

(find-goal)
