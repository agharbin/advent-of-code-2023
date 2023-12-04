(ns adamharbin.advent.2023.3.1
  (:require [clojure.set :as cset]))

;; Input Parsing

(def digit #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
(def valid-symbols #{\* \- \$ \= \+ \& \@ \# \/ \%})

(defn parse-input [input]
  (loop [i 0
         row 0
         col 0
         symbols #{}
         numbers []
         num-buffer []]
    (if (<= (count input) i)
      {:symbols symbols :numbers numbers}
      (let [c (input i)
            next-i (inc i)
            next-row (if (= c \newline) (inc row) row)
            next-col (if (= c \newline) 0 (inc col))
            next-symbols (if (valid-symbols c) (conj symbols [row col]) symbols)
            next-numbers (if (and (not (empty? num-buffer)) (not (digit c)))
                           (conj numbers [(apply str num-buffer) [row col]])
                           numbers)
            next-num-buffer (if (digit c)
                             (conj num-buffer c)
                             [])]
        (recur next-i next-row next-col next-symbols next-numbers next-num-buffer)))))

;; Logic

(defn surrounding-locs [[num-string [end-row end-col]]]
  (into #{}
    (for [row [(dec end-row) end-row (inc end-row)]
          col (range (- end-col (count num-string) 1) (inc end-col))]
      [row col])))

(defn touches-symbol? [number symbols]
  (not (empty? (cset/intersection (surrounding-locs number) symbols))))

(defn solve [{symbols :symbols numbers :numbers}]
  (->> numbers
       (filter #(touches-symbol? % symbols))
       (map (fn [[num-str _]] (parse-long num-str)))
       (apply +)))

(def raw-input (vec (slurp "sample.dat")))
(def parsed-input (parse-input raw-input))
(solve parsed-input)
