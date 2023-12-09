(ns adamharbin.advent.2023.8
  (:require [clojure.string :as s]))

;; Input Parsing

(def re #"(.*) = \((.*), (.*)\)")

(defn parse-nodes [input]
  (let [[_ node left right] (re-matches re input)]
    [node [left right]]))

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
         move-count 0]
    (if (= position "ZZZ")
      move-count
      (recur (next ts)
             (turn (first ts) position nodes)
             (inc move-count)))))

(solve parsed-input)

;; Part 2

(defn ends-with-z? [nodes]
  (= \Z (nth nodes 2)))

(defn find-z [{turns :turns nodes :nodes} start]
  (loop [ts (cycle turns)
         position start
         move-count 0]
    (if (ends-with-z? position)
      move-count
      (recur (rest ts)
             (turn (first ts) position nodes)
             (inc move-count)))))

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
