(ns adamharbin.advent.2023.2.1
  (:require [clojure.string :as s]))

;; Input Parsing

(def game-num-results-regex #"Game (.*): (.*)") ; Matches "Game 9: 1 red, 2 blue, 3 green; 4 red, 5 blue, 6 green"
(def game-regex #"[0-9]+ [a-z]+") ; Matches "1 red, 2 blue, 3 green" when used with re-seq

(defn parse-game [input]
  (->> input ; "1 red, 2 blue, 3 green"
       (re-seq game-regex) ; -> ["1 red" "2 blue" "3 green"]
       (map #(s/split % #" ")) ; [["1" "red"] ["2" "blue"] ["3" "green"]]
       (map (fn [[count color]] [(parse-long count) (keyword color)])))) ; -> [[1 :red] [2 :blue] [3 :green]]

(defn parse-line [input]
  (let [[_ game-num results] (re-matches game-num-results-regex input)
        game-strings (s/split results #";")
        games (map parse-game game-strings)]
    [(parse-long game-num) games]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(def red-limit 12)
(def green-limit 13)
(def blue-limit 14)

(defn color-below-limit [[n color]]
  (case color
    :red (<= n red-limit)
    :green (<= n green-limit)
    :blue (<= n blue-limit)
    "Invalid Color"))

(defn below-limits? [cube-counts-vec]
  (every? color-below-limit cube-counts-vec))

(defn legal-game? [input]
  (let [[_ results-vec] input]
    (every? below-limits? results-vec)))

(defn solve [input]
  (->> input
       (filter legal-game?)
       (map first)
       (apply +)))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))
(solve parsed-input)
