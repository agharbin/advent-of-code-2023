(ns adamharbin.advent.2023.16
  (:require [clojure.string :as s]
            [adamharbin.advent.common :refer [vector+]]))

;; Input Parsing

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv vec)))

(def raw-input (slurp "input.dat"))
(def grid (parse-input raw-input))
(def grid-rows (count grid))
(def grid-cols (count (first grid)))

;; Part 1

(def reflect-back-slash
  {[1 0] [0 1]
   [0 1] [1 0]
   [-1 0] [0 -1]
   [0 -1] [-1 0]})

(def reflect-forward-slash
  {[1 0] [0 -1]
   [0 -1] [1 0]
   [-1 0] [0 1]
   [0 1] [-1 0]})

(defn move-beam [{curr-loc :curr-loc, momentum :momentum}]
  "Returns next (position, momentum) for a given light beam. We return a vector
   since splitters can cause two beams to result from one move."
  (let [next-loc (vector+ curr-loc momentum)
        tile (get-in grid next-loc)
        next-beam {:curr-loc next-loc}]
    (if (nil? tile) ; Outisde of grid. No beam results.
      []
      (case tile
        \. [(assoc next-beam :momentum momentum)]
        \\ [(assoc next-beam :momentum (reflect-back-slash momentum))]
        \/ [(assoc next-beam :momentum (reflect-forward-slash momentum))]
        \| (if (#{[0 1] [0 -1]} momentum)
             [(assoc next-beam :momentum (reflect-back-slash momentum))
              (assoc next-beam :momentum (reflect-forward-slash momentum))]
             [(assoc next-beam :momentum momentum)])
        \- (if (#{[1 0] [-1 0]} momentum)
             [(assoc next-beam :momentum (reflect-back-slash momentum))
              (assoc next-beam :momentum (reflect-forward-slash momentum))]
             [(assoc next-beam :momentum momentum)])))))

(defn move [[seen beams]]
  "Applies movement logic to all current beams and remembers all (position, momentum)
   pairs. Importantly, we discard any (position, momentum) pair previously seen
   since these can grow infinitely, but can't result in any new spaces being traversed."
  (let [next-beams (mapcat move-beam beams)
        next-seen (into seen beams)]
    [next-seen (into #{} (filter #(not (seen %)) next-beams))]))

; My input has a \ at position [0 0], this is a hack to allow for the fact that
; this code doesn't process collision logic on move 1.
(def start-state [#{} #{{:curr-loc [0 0] :momentum [1 0]}}])

(defn compute-energized [start-state]
  (let [final-state (->> start-state
                         (iterate move)
                         (drop-while #(not (empty? (second %)))) ; more beams to simulate
                         first)
        beam-states-seen (first final-state)
        unique-locations (into #{} (map :curr-loc beam-states-seen))]
    (count unique-locations)))

(compute-energized start-state)

;; Part 2

(def start-states
  (concat
    (for [c (range grid-cols)] [#{} #{{:curr-loc [0 c] :momentum [1 0]}}])
    (for [c (range grid-cols)] [#{} #{{:curr-loc [(dec grid-rows) c] :momentum [-1 0]}}])
    (for [r (range grid-rows)] [#{} #{{:curr-loc [r 0] :momentum [0 1]}}])
    (for [r (range grid-rows)] [#{} #{{:curr-loc [r (dec grid-cols)] :momentum [0 -1]}}])))

(apply max (map compute-energized start-states))
