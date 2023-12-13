(ns adamharbin.advent.2023.12
  (:require [clojure.string :as s]))

;; Input Parsing

(defn parse-line [input]
  (let [[springs groups] (s/split input #" ")
        group-vec (map parse-long (s/split groups #","))]
    [springs group-vec]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))

;; Logic

(def solve-case
  (memoize
    (fn [[springs groups]]
      (let [match-group
              (fn [springs groups]
                (if (and (every? #{\# \?} (take (first groups) springs))
                         (#{\. \?} (first (drop (first groups) springs))))
                  (solve-case [(drop (inc (first groups)) springs) (drop 1 groups)])
                  0))
            match-space
              (fn [springs groups]
                (solve-case [(drop 1 springs) groups]))]
        (cond
          (and (empty? groups) (every? #{\. \?} springs)) 1
          (empty? groups) 0
          (empty? springs) 0
          :else (case (first springs)
                  \. (match-space springs groups)
                  \# (match-group springs groups)
                  \? (+
                       (match-space springs groups)
                       (match-group springs groups))))))))

(defn append-tail-space-and-solve-case [[springs groups]]
  (solve-case [(concat springs '(\.)) groups]))

(defn solve [input]
  (apply + (map append-tail-space-and-solve-case input)))

(solve parsed-input)

(defn expand [[springs groups]]
  [(s/join "?" (repeat 5 springs))
   (flatten (repeat 5 groups))])

(defn solve-2 [input]
  (->> input
       (map expand)
       (map append-tail-space-and-solve-case)
       (apply +)))

(solve-2 parsed-input)
