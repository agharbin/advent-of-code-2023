(ns adamharbin.advent.2023.8
  (:require [clojure.string :as s]))

;; Input Parsing

(def re #"(.*) = \((.*), (.*)\)")

(defn parse-nodes [input]
  (let [[_ a l r] (re-matches re input)]
    [a [l r]]))

(defn parse-input [input]
  (let [[turns nodes-string] (s/split input #"\n\n")
        nodes (into {} (map parse-nodes (s/split-lines nodes-string)))]
    {:turns turns :nodes nodes}))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))

;; Part 1

(defn turn [direction curr-position nodes]
  (case direction
    \L (first (nodes curr-position))
    \R (second (nodes curr-position))))

(defn solve [{turns :turns nodes :nodes}]
  (loop [ts (cycle turns)
         position "AAA"
         moves 0]
    (if (= position "ZZZ")
      moves
      (recur (next ts)
             (turn (first ts) position nodes)
             (inc moves)))))

(solve parsed-input)

;; Part 2

(defn ends-with-z? [nodes]
  (every? #(= \Z (nth % 2)) nodes))

(defn find-z [{turns :turns nodes :nodes} start]
  (let [starting-nodes (filter #(= \A (nth % 2)) (keys nodes))]
    (loop [ts (cycle turns)
           positions [start]
           moves 0]
      (if (ends-with-z? positions)
        moves
        (recur (rest ts)
               (mapv (fn [x] (turn (first ts) x nodes)) positions)
               (inc moves))))))

(defn gcd [a b]
  (if (not= b 0)
    (recur b (mod a b))
    a))

(defn lcm [a b]
  (/ (* a b)
     (gcd a b)))

(def starts-with-a (filter #(= \A (nth % 2)) (keys (:nodes parsed-input))))
(def dists (map #(find-z parsed-input %) starts-with-a))
(reduce lcm dists)
