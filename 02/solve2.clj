(ns adamharbin.advent.2023.2.2
  (:require [clojure.string :as s]))

;; Input Parsing

(def game-num-results-regex #"Game (.*): (.*)") ; Matches "Game 9: 1 red, 2 blue, 3 green; 4 red, 5 blue, 6 green"
(def game-regex #"[0-9]+ [a-z]+") ; Matches "1 red, 2 blue, 3 green" when used with re-seq

(defn parse-game [input]
  (->> input ; "1 red, 2 blue, 3 green"
       (re-seq game-regex) ; -> ["1 red" "2 blue" "3 green"]
       (map #(s/split % #" ")) ; [["1" "red"] ["2" "blue"] ["3" "green"]]
       (map (fn [[count color]] [(keyword color) (parse-long count)])) ; -> [[:red 1] [:blue 2] [:green 3]]
       (into {}))) ; -> {:red 1, :blue 2, :green 3}

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

(defn cube-value [[_ results]]
  (let [max-vals (apply merge-with max results)]
    (* (:red max-vals) (:green max-vals) (:blue max-vals))))

(defn solve [input]
  (->> input
       (map cube-value)
       (apply +)))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))
(solve parsed-input)
