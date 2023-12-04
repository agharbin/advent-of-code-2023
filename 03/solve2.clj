(ns adamharbin.advent.2023.3.2
  (:require [clojure.set :as cset]))

(def digit #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
(def valid-symbols #{\- \$ \= \+ \& \@ \# \/ \%})

(defn parse-input [input]
  (loop [i 0
         row 0
         col 0
         symbols #{}
         gears #{}
         numbers []
         num-buffer []]
    (if (<= (count input) i)
      {:symbols symbols :numbers numbers :gears gears}
      (let [c (input i)
            next-i (inc i)
            next-row (if (= c \newline) (inc row) row)
            next-col (if (= c \newline) 0 (inc col))
            next-gears (if (= c \*) (conj gears [row col]) gears)
            next-symbols (if (valid-symbols c) (conj symbols [row col]) symbols)
            next-numbers (if (and (not (empty? num-buffer)) (not (digit c)))
                           (conj numbers [(apply str num-buffer) [row col]])
                           numbers)
            next-num-buffer (if (digit c)
                             (conj num-buffer c)
                             [])]
        (recur next-i next-row next-col next-symbols
               next-gears next-numbers next-num-buffer)))))

(defn surrounding-locs [[num-string [end-row end-col]]]
  (into #{}
    (for [row [(dec end-row) end-row (inc end-row)]
          col (range (- end-col (count num-string) 1) (inc end-col))]
      [row col])))

(defn touches-gear? [number gear]
  (not (empty? (cset/intersection (surrounding-locs number) #{gear}))))

(defn solve [{numbers :numbers gears :gears}]
  (->> (for [gear gears
             n1 (filter #(touches-gear? % gear) numbers)
             n2 (filter #(touches-gear? % gear) numbers)
             :when (not= n1 n2)]
         (* (parse-long (first n1)) (parse-long (first n2))))
       (apply +)
       (* 1/2))) ; Need this 1/2 factor due to double counting of the above pairs

(def raw-input (vec (slurp "input.dat")))
(def parsed-input (parse-input raw-input))
(solve parsed-input)
