(ns adamharbin.advent.2023.23
  (:require [clojure.string :as s]
            [clojure.set :as cset]
            [adamharbin.advent.common :refer [vector+]]))

;; Parse Input

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv vec)))

(def raw-input (slurp "input.dat"))
(def grid (parse-input raw-input))
(def grid-rows (count grid))
(def grid-cols (count (first grid)))

;; Part 1

(def start-pos [0 1])
(def goal-pos [(- grid-rows 1) (- grid-cols 2)])

(defn find-next-legal-positions [pos seen-pos]
  "Find neighboring tiles that we can legally move to from here."
  (let [candidates (case (get-in grid pos)
                     \> [(vector+ pos [0 1])]
                     \v [(vector+ pos [1 0])]
                     \. (for [offset [[1 0] [0 1] [-1 0] [0 -1]]] (vector+ pos offset)))]
    (->> candidates
         (filter #(#{\. \> \v} (get-in grid %)))
         (filter #(not (seen-pos %))))))

(defn solve [pos seen]
  "Depth-first search of grid to find longest path."
  (let [to-explore (find-next-legal-positions pos seen)]
    (cond
      (= goal-pos pos) 0
      (empty? to-explore) Integer/MIN_VALUE
      :else
        (->> to-explore
             (map #(solve % (conj seen pos)))
             (apply max)
             inc))))

(solve start-pos #{})

;; Part 2

(defn find-next-legal-positions-2 [pos]
  "Find next legal moves for part2 (ignores slopes)."
  (let [candidates (for [offset [[1 0] [0 1] [-1 0] [0 -1]]] (vector+ pos offset))]
    (->> candidates
         (filter #(#{\. \> \v} (get-in grid %))))))

(defn find-junctions []
  "Find all spaces where 2 or more paths meet."
  (->> (for [r (range grid-rows) c (range grid-cols)] [r c])
       (filter #(#{\.} (get-in grid %)))
       (map (fn [pos] [pos (find-next-legal-positions-2 pos)]))
       (filter #(<= 3 (count (second %))))
       (map first)
       (into #{})
       (cset/union #{start-pos goal-pos})))

(def junctions (find-junctions))

(defn find-connections [pos]
  "Breadth-first search from a junction to find its nearest neighbor junction
   down each path and the corresponding distance."
  (let [paths (->> pos
                   find-next-legal-positions-2
                   (filter #(#{\. \> \v} (get-in grid %))))]
    (loop [q (into clojure.lang.PersistentQueue/EMPTY (for [p paths] [p 0]))
           seen #{pos}
           neighbors #{}]
      (if (seq q)
        (let [[state dist] (peek q)]
          (cond
            (seen state) (recur (pop q) seen neighbors)
            (junctions state) (recur (pop q) seen (conj neighbors [state (inc dist)]))
            :else
              (let [candidates (find-next-legal-positions-2 state)]
                (recur (into (pop q) (for [s candidates] [s (inc dist)]))
                       (conj seen state)
                       neighbors))))
        neighbors))))

(defn build-distance-map []
  "For each pair of 'adjacent' junctions, create a map of the distances."
  (into {}
    (for [j junctions [neighbor dist] (find-connections j)]
      [[j neighbor] dist])))

;; Map of [position-1 position] => distance, where positions are junctions
(def distance-map (build-distance-map))

(defn solve-2 [pos seen-pos]
  "Depth-first search of grid to find longest path, this time considering
   only forks in the path rather than each step."
  (let [to-explore (->> (keys distance-map)
                        (filter (fn [[p _]] (= p pos)))
                        (map second)
                        (filter #(not (seen-pos %))))]
    (cond
      (= goal-pos pos) 0
      (empty? to-explore) Integer/MIN_VALUE
      :else
        (->> to-explore
             (map #(+ (distance-map [pos %])
                      (solve-2 % (conj seen-pos pos))))
             (apply max)))))

(solve-2 [0 1] #{})
