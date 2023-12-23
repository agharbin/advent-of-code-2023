(ns adamharbin.advent.2023.21
  (:require [clojure.string :as s]
            [clojure.set :as cset]
            [adamharbin.advent.common :refer [vector+]]))

;; Parse Input

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv vec)))

(def raw-input (slurp "input.dat"))
(def grid (parse-input raw-input))
(def row-count (count grid))
(def col-count (count (first grid)))

;; Part 1

(def start-state (->> (for [i (range row-count) j (range col-count)] [i j])
                      (filter #(= \S (get-in grid %)))
                      first))

(defn wrap-coords [[r c]]
  [(mod r row-count) (mod c col-count)])

(defn possible-next-states [s]
  (filter #(not= \# (get-in grid (wrap-coords %)))
            (for [d [[1 0] [-1 0] [0 1] [0 -1]]] (vector+ s d))))

(defn bfs [steps]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [0 start-state])
         reachable #{}]
    (if (seq q)
      (let [[dist state] (peek q)]
        (cond
          (< steps dist) (recur (pop q) reachable)
          (reachable state) (recur (pop q) reachable)
          :else
            (let [candidates (possible-next-states state)]
              (recur
                (into (pop q) (for [s candidates] [(inc dist) s]))
                (if (or (and (even? steps) (even? dist))
                        (and (odd? steps) (odd? dist)))
                  (conj reachable state)
                  reachable)))))
      (count reachable))))

(bfs 64)

;; Part 2

(defn lagrange [xs ys]
  "Implements Lagrange interpolation for given vectors of x-values and y-values."
  (let [k (count xs)
        zero-to-k (range 0 k)]
    (fn [x]
      (apply +
        (for [j zero-to-k]
          (* (ys j)
             (apply *
               (for [m zero-to-k :when (not= j m)]
                 (/ (- x (xs m)) (- (xs j) (xs m)))))))))))

;; The input has a diamond shape with 65 spaces (half of grid width + 1) from the
;; origin to the edge. After expanding outward to this edge, the output enters a
;; repeating pattern every 131 (width of grid) steps. We find a degree 2 polynomial
;; to describe this pattern.

(def xs [0 1 2])
(def ys (mapv #(bfs (+ 65 (* % 131))) xs))
(def solution (langrange xs ys))
(def step-count 26501365)
(def iterations (/ (- step-count 65) 131))

(solution iterations)
