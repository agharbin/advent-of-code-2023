(ns adamharbin.advent.2023.13
  (:require [clojure.string :as s]
            [adamharbin.advent.common :refer [zip]]
            [adamharbin.advent.matrix :refer [transpose]]))

;; Input Parsing

(defn parse-board [input]
  (->> input
       s/split-lines))

(defn parse-input [input]
  (->> (s/split input #"\n\n")
       (map parse-board)))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))

;; Logic

(defn find-horizontal-index [input test-fn]
  (loop [curr-index 1]
    (if (= (count input) curr-index)
      nil
      (let [prefix (take curr-index input)
            suffix (drop curr-index input)
            size (min (count prefix) (count suffix))]
        (if (test-fn (take size (reverse prefix))
                     (take size suffix))
          curr-index
          (recur (inc curr-index)))))))

(defn find-vertical-index [input test-fn]
  (find-horizontal-index (transpose input) test-fn))

(defn solve-board [input test-fn]
  (let [horizontal-index (find-horizontal-index input test-fn)
        vertical-index (find-vertical-index input test-fn)]
    (if (some? horizontal-index)
      (* 100 horizontal-index)
      vertical-index)))

(defn solve [input test-fn]
  (->> input
       (map #(solve-board % test-fn))
       (apply +)))

(solve parsed-input =)

(defn same? [[x y]] (= x y))

(defn one-difference? [image1 image2]
  (let [image1-chars (flatten (map seq image1))
        image2-chars (flatten (map seq image2))]
    (->> (for [pair (zip image1-chars image2-chars)] (if (same? pair) 0 1))
         (apply +)
         (= 1))))

(solve parsed-input one-difference?)
