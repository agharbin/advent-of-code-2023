(ns adamharbin.advent.2023.25
  (:require [clojure.string :as s]
            [clojure.set :as cset]))

;; Parse Input

(defn parse-line [input]
  (let [[_ src destination-str] (re-matches #"(\w+): (.*)" input)
        destinations (->> (re-seq #"(\w+)" destination-str) (mapcat #(drop 1 %)))]
    [src destinations]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(def raw-input (slurp "input.dat"))
(def graph (parse-input raw-input))

;; Part 1

(defn find-edges []
  "Convert from [source [dest1 dest2 ..]] to #{src dest} sets"
  (let [source->dest (into {} graph)]
    (for [v1 (keys source->dest) v2 (source->dest v1)] #{v1 v2})))

(def edges (into #{} (find-edges)))

(defn generate-dot-output []
  "Render the graph in GraphViz text format for visualization."
  (let [edge-strings (map #(apply format "%s -- %s" %) edges)
        joined-edge-strings (s/join \newline edge-strings)]
    (str "graph { \n" joined-edge-strings "\n}")))

(spit "graph.dot" (generate-dot-output))

;; These were determined by manual inspection of graph visualization.
(def edges-to-remove #{#{"cnr" "hcd"} #{"fhv" "zsp"} #{"bqp" "fqr"}})

(def new-edges (cset/difference edges edges-to-remove))

(defn fill-partition [initial-partition]
  "Given an arbitrarily chosen node, fill out the partition it belongs to by recursively adding
   new vertices that connect to the current partition."
  (let [matching-edges (filter #(not (empty? (cset/intersection initial-partition %))) new-edges)
        new-vertices (cset/difference (into initial-partition (apply concat matching-edges))
                                      initial-partition)]
    (if (empty? new-vertices)
      initial-partition
      (recur (cset/union initial-partition new-vertices)))))

(def size-of-first-partition (count (fill-partition #{"mnf"})))
(def total-vertex-count (count (into #{} (apply concat edges))))
(def size-of-second-partition (- total-vertex-count size-of-first-partition))

(* size-of-first-partition size-of-second-partition)
