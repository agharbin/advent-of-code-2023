(ns adamharbin.advent.2023.11
  (:require [clojure.string :as s]
            [adamharbin.advent.common :refer [zip]]
            [adamharbin.advent.matrix :refer [transpose]]))

;; Input Parsing

(defn parse-line [input]
  (vec input))

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv parse-line)))

;; Logic

(defn no-galaxies? [input]
  (every? #{\.} input))

(defn find-empty-rows [input]
  (->> (zip input (range (count input)))
       (filter #(no-galaxies? (first %)))
       (map second)))

(defn find-empty-cols [input]
  (find-empty-rows (transpose input)))

(defn find-galaxies [input]
  (->> (for [row (range (count input))
             col (range (count (first input)))]
         [row col])
       (filter #(= \# (get-in input %)))))

(defn compute-distance [[r1 c1] [r2 c2]]
  (+ (abs (- r1 r2)) (abs (- c1 c2))))

(defn compute-empty-rows-between [empty-rows [r1 _] [r2 _] mult]
  (let [[r1' r2'] (sort [r1 r2])]
    (* mult
       (count (filter #(< r1' % r2') empty-rows)))))

(defn compute-empty-cols-between [empty-cols [_ c1] [_ c2] mult]
  (let [[c1' c2'] (sort [c1 c2])]
    (* mult
       (count (filter #(< c1' % c2') empty-cols)))))

(defn solve [input multiplier]
  (let [empty-rows (find-empty-rows input)
        empty-cols (find-empty-cols input)
        galaxies (find-galaxies input)]
    (-> (apply +
          (for [g galaxies h galaxies :when (not= g h)]
            (+ (compute-distance g h)
               (compute-empty-rows-between empty-rows g h multiplier)
               (compute-empty-cols-between empty-cols g h multiplier))))
        (/ 2))))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))
(solve parsed-input 999999)
