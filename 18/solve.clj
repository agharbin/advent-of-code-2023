(ns adamharbin.advent.2023.18.1
  (:require [clojure.string :as s]
            [clojure.set :as cset]
            [adamharbin.advent.common :refer [vector+]]))

;; Parse Input

(defn parse-line [input]
  (let [[_ dir-str length-str color-str] (re-matches #"(.) (.*) \(#*(.*)\)" input)]
    [(keyword dir-str) (parse-long length-str) color-str]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))

;; Part 1

(defn find-offset [[dir length _]]
  "Offset position by given length in the given direction."
  (case dir
    :U [(- length) 0]
    :D [length 0]
    :L [0 (- length)]
    :R [0 length]))

(defn find-between [[r c] [r' c']]
  "Find the set of positions encompassed by the given range."
  (let [min-r (min r r')
        max-r (max r r')
        min-c (min c c')
        max-c (max c c')]
    (for [i (range min-r (inc max-r)) j (range min-c (inc max-c))] [i j])))

(def start-pos [0 0])

(defn find-perimeter [input]
  "Find the perimeter locations of the shape given the input path."
  (loop [xs input
         curr-pos start-pos
         seen #{start-pos}]
    (if (seq xs)
      (let [next-move (first xs)
            next-pos (vector+ curr-pos (find-offset next-move))
            positions-between (find-between curr-pos next-pos)]
        (recur (rest xs) next-pos (into seen positions-between)))
      seen)))

(defn possible-next-states [perimeter [r c :as loc]]
  (if (perimeter loc)
    []
    (for [dr [-1 0 1] dc [-1 0 1]] [(+ r dr) (+ c dc)])))

(defn bfs [input]
  "Use breadth-first search to find all points in the interior defined by
   the given path."
  (let [perimeter (find-perimeter input)]
    (loop [q (conj clojure.lang.PersistentQueue/EMPTY [1 1])
           seen-states #{}]
        (if (seq q)
          (let [state (peek q)]
            (if (seen-states state)
              (recur (pop q) seen-states)
              (let [candidates (possible-next-states perimeter state)]
                (recur
                  (into (pop q) candidates)
                  (conj seen-states state)))))
          (count (cset/union seen-states perimeter))))))

(bfs parsed-input)
