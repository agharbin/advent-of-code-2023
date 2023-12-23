(ns adamharbin.advent.2023.20
  (:require [clojure.string :as s]))

;; Parse Input

(defn parse-line [input]
  "Converts e.g. '%nr -> hq' to [:flip :nr [:hq]]"
  (let [[_ prefix label target-string] (re-matches #"([%&])?(.+) -> (.*)" input)
        targets (map s/trim (s/split target-string #","))]
    (case prefix
      nil [:node (keyword label) (mapv keyword targets)]
      "%" [:flip (keyword label) (mapv keyword targets)]
      "&" [:conj (keyword label) (mapv keyword targets)])))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))

;; Part 1

(def node-type first)
(def node-label second)
(defn node-connections [xs] (nth xs 2))

(defn incoming-nodes [label]
  "Find and return all nodes that feed into the given node."
  (->> (for [node parsed-input connection (node-connections node)] [(node-label node) connection])
       (filter #(= label (second %)))
       (map first)))

(defn find-initial-state []
  "Calculates the initial values of all stateful nodes (& and %). We represent the state
   as a pair of dictionaries mapping the node label to the current state."
  (let [flip-flops (filter #(= :flip (node-type %)) parsed-input)
        flip-flop-labels (map node-label flip-flops)
        flip-flop-state (into {} (for [l flip-flop-labels] [l :off]))
        conjunctions (filter #(= :conj (node-type %)) parsed-input)
        conj-labels (map node-label conjunctions)
        conj-state (into {} (for [l conj-labels] [l (into {} (for [c (incoming-nodes l)] [c :low]))]))]
    [flip-flop-state conj-state]))

(def initial-state (find-initial-state))

(defn build-neighbors-map []
  "Build a dictionary of node labels to downtream neighbors."
  (into {} (for [node parsed-input] [(node-label node) (node-connections node)])))

(def neighbors (build-neighbors-map))

(defn process-signal [[flip-s conj-s :as state] [source strength target :as signal]]
  "Given a current state and new signal, compute the next state and any newly generated signals.
   We return a pair [new-state new-signals]."
  (cond
    (= target :broadcaster)
      [state (for [n (neighbors target)] [target :low n])]
    (flip-s target)
      (if (= :low strength)
        (if (= :off (flip-s target))
          [[(assoc flip-s target :on) conj-s] (for [n (neighbors target)] [target :high n])]
          [[(assoc flip-s target :off) conj-s] (for [n (neighbors target)] [target :low n])])
        [state []])
    (conj-s target)
      (let [next-conj-s (assoc-in conj-s [target source] strength)]
        (if (every? #(= :high %) (vals (next-conj-s target)))
          [[flip-s next-conj-s] (for [n (neighbors target)] [target :low n])]
          [[flip-s next-conj-s] (for [n (neighbors target)] [target :high n])]))
    :else [state []]))

(def initial-queue (conj clojure.lang.PersistentQueue/EMPTY [:button :low :broadcaster]))

(defn solve []
  "Count the total number of signals produced after 1000 iterations."
  (loop [state initial-state
         signals initial-queue
         low-signal-count 0
         high-signal-count 0
         iterations 1000]
    (if (zero? iterations)
      (* low-signal-count high-signal-count)
      (if (seq signals)
        (let [[next-state new-signals] (process-signal state (peek signals))
              new-low-signals (filter #(= :low (second %)) new-signals)
              new-high-signals (filter #(= :high (second %)) new-signals)]
          (recur
            next-state
            (into (pop signals) new-signals)
            (+ low-signal-count (count new-low-signals))
            (+ high-signal-count (count new-high-signals))
            iterations))
        (recur
          state
          initial-queue
          (inc low-signal-count)
          high-signal-count
          (dec iterations))))))

(solve)

;; Part 2

(defn presses-until-low-send [label]
  "Counts the number of times we need to press the button before the specified node
   sends a 'low' signal."
  (loop [state initial-state
         signals initial-queue
         presses 1]
    (if (seq signals)
      (if (= [label :low] (take 2 (peek signals)))
        presses
        (let [[next-state new-signals] (process-signal state (peek signals))]
          (recur next-state (into (pop signals) new-signals) presses)))
      (recur state initial-queue (inc presses)))))

;; There are a set of 4 '&' nodes that feed into the target :rx node.
;; Each of these 4 nodes triggers in a cycle of a different length (mutually prime).
;; The target node will trigger when all 4 of these nodes trigger simultaneously, so
;; we compute the overall number of iterations as the product of these cycle lengths.

(*
  (presses-until-low-send :qr)
  (presses-until-low-send :lk)
  (presses-until-low-send :ft)
  (presses-until-low-send :lz))
