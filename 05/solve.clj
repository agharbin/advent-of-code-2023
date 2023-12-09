(ns adamharbin.advent.2023.5.1
  (:require [clojure.string :as s]))

;; Input Parsing

(defn string-list-to-longs [input]
  (map parse-long (s/split input #" ")))

(defn parse-ranges [range-str]
  (let [range-strings (drop 1 (s/split-lines range-str))]
    (map string-list-to-longs range-strings)))

(defn parse-seeds [input]
  (map parse-long (re-seq #"\d+" input)))

(defn parse-input [input]
  (let [blocks (s/split input #"\n\n")
        [seeds-str map1-str map2-str map3-str map4-str map5-str map6-str map7-str] blocks
        seeds (parse-seeds seeds-str)
        map1 (parse-ranges map1-str)
        map2 (parse-ranges map2-str)
        map3 (parse-ranges map3-str)
        map4 (parse-ranges map4-str)
        map5 (parse-ranges map5-str)
        map6 (parse-ranges map6-str)
        map7 (parse-ranges map7-str)]
    [seeds map1 map2 map3 map4 map5 map6 map7]))

;; Logic

(defn mapping [ranges]
  (fn [x]
    (loop [rs ranges]
      (if (seq rs)
        (let [[target source dist] (first rs)]
          (if (<= source x (+ source dist -1))
            (+ x (- target source))
            (recur (rest rs))))
        x))))

(defn solve [[seeds m1 m2 m3 m4 m5 m6 m7]]
  (apply min
    (for [s seeds]
      ((comp
         (mapping m7)
         (mapping m6)
         (mapping m5)
         (mapping m4)
         (mapping m3)
         (mapping m2)
         (mapping m1))
       s))))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))
(solve parsed-input)
