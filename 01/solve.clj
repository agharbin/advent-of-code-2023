(ns adamharbin.advent.2023.1
  (:require [clojure.string :as s]))

(def forward-regex #"[0-9]|one|two|three|four|five|six|seven|eight|nine")
(def backward-regex #"[0-9]|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin")

(def digit-names
  {
    "one" "1"
    "eno" "1"
    "two" "2"
    "owt" "2"
    "three" "3"
    "eerht" "3"
    "four" "4"
    "ruof" "4"
    "five" "5"
    "evif" "5"
    "six" "6"
    "xis" "6"
    "seven" "7"
    "neves" "7"
    "eight" "8"
    "thgie" "8"
    "nine" "9"
    "enin" "9"
  })

(defn parse-line [input]
  (let [f (first (re-seq forward-regex input))
        l (first (re-seq backward-regex (apply str (reverse input))))]
    (parse-long (str (get digit-names f f) (get digit-names l l)))))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(defn solve [input]
  (apply + input))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))
(solve parsed-input)
