(ns adamharbin.advent.2023.5.2
  (:require [clojure.string :as s]))

;; Input Parsing

(defn string-list-to-longs [input]
  (map parse-long (s/split input #" ")))

(defn parse-ranges [range-str]
  (let [range-strings (drop 1 (s/split-lines range-str))]
    (->> range-strings
         (map string-list-to-longs)
         ;; Convert from given format to [offset start end]
         (map (fn [[t s l]] [(- t s) s (+ s l)])))))

(defn parse-seeds [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
       (partition 2)
       (map (fn [[s l]] [s (+ s l)]))))

(defn parse-input [input]
  (let [blocks (s/split input #"\n\n")
        [seeds-str map1-str map2-str map3-str
         map4-str map5-str map6-str map7-str] blocks
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

(defn apply-mapping [mappings]
  (fn [[start end]]
    (let [mapping-start-ends (->> mappings (map #(drop 1 %)))
          range-boundaries (->> mapping-start-ends
                                (apply concat)
                                (into #{}))
          resulting-ranges (->> (into range-boundaries [start end])
                                (into #{})
                                sort
                                (partition 2 1)
                                (filter (fn [[s e]] (<= start s e end))))]
      (map
        (fn [[start end]]
          (let [seeds-within-range? (fn [[_ rstart rend]] (<= rstart start end rend))
                matching-ranges (filter seeds-within-range? mappings)]
            (if (empty? matching-ranges)
              [start end] ; Seed range didn't fall within this mapping
              (let [offset (first (first matching-ranges))]
                [(+ offset start) (+ offset end)]))))
        resulting-ranges))))

(defn solve [[seeds m1 m2 m3 m4 m5 m6 m7]]
  (sort-by first
    (loop [ms [m1 m2 m3 m4 m5 m6 m7]
           ss seeds]
      (if (seq ms)
        (recur (rest ms) (mapcat (apply-mapping (first ms)) ss))
        ss))))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))
(-> parsed-input solve first first)
