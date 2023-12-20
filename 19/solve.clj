(ns adamharbin.advent.2023.19
  (:require [clojure.string :as s]
            [clojure.core.match :as m]))

;; Parse Input

(defn parse-rule [[_ feature op value result]]
  [(keyword feature) (first op) (parse-long value) (keyword result)])

(defn parse-flow [input]
  "Parses flow rule format. E.g. 'b:{a>100:R,A}' to [:b [[:a \\> 100 :R] :A]]"
  (let [[_ label rules-str] (re-matches #"(\w+)\{(.*)\}" input)
        rules-strings (re-seq #"(\w)([<>])(\d+):(\w+)," rules-str)
        rules (map parse-rule rules-strings)
        final-rule (keyword (last (s/split rules-str #",")))]
    [(keyword label) (vec (concat rules [final-rule]))]))

(defn parse-part [input]
  "Parses part specification. E.g. {x=3,m=4,a=5,s=6} to {:x 3, :m 4, :a 5, :s 6}"
  (let [[_ x m a s] (re-matches #"\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}" input)]
    {:x (parse-long x)
     :m (parse-long m)
     :a (parse-long a)
     :s (parse-long s)}))

(defn parse-input [input]
  "Splits given input into 2 blocks (flow and parts) and parses each individually."
  (let [[flows parts] (s/split input #"\n\n")]
    {:flows (->> flows s/split-lines (map parse-flow) (into {}))
     :parts (->> parts s/split-lines (map parse-part))}))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))
(def flows (parsed-input :flows))
(def parts (parsed-input :parts))

;; Part 1

(defn accepted? [part flow-name]
  "Test if a given part is accepted according to the workflow rules."
  (case flow-name
    :R false
    :A true
    (let [flow (flows flow-name)]
      (loop [xs flow]
        (m/match (first xs)
          [feature op value result]
            (case op
              \> (if (> (part feature) value) (accepted? part result) (recur (rest xs)))
              \< (if (< (part feature) value) (accepted? part result) (recur (rest xs))))
          result (accepted? part result))))))

(defn compute-score [part]
  "Compute the score of an accepted part."
  (apply + (vals part)))

(defn solve []
  (->> parts
       (filter #(accepted? % :in))
       (map compute-score)
       (apply +)))

(solve)

;; Part 2

(defn score-interval [[[x1 x2] [m1 m2] [a1 a2] [s1 s2]]]
  "Compute the score of a range of parts, given as [start, end) pairs for each x,m,a and s"
  (* (- x2 x1) (- m2 m1) (- a2 a1) (- s2 s1)))

(defn split-upper [interval feature value]
  "Splits an interval on the given dimention (feature) at value and takes the upper part."
  (let [idx ({:x 0 :m 1 :a 2 :s 3} feature)
        [_ e] (interval idx)]
    (assoc interval idx [value e])))

(defn split-lower [interval feature value]
  "Splits an interval on the given dimention (feature) at value and takes the lower part."
  (let [idx ({:x 0 :m 1 :a 2 :s 3} feature)
        [s _] (interval idx)]
    (assoc interval idx [s value])))

(defn solve-interval [interval flow-name i]
  "Recursively score the workflows on the input interval, splitting intervals whenever
   we reach a branch. Return the final score."
  (case flow-name
    :R 0
    :A (score-interval interval)
    (let [rule ((flows flow-name) i)]
      (m/match rule
        [feature op value result]
          (case op
            \> (+
                 (solve-interval (split-upper interval feature (inc value)) result 0)
                 (solve-interval (split-lower interval feature (inc value)) flow-name (inc i)))
            \< (+
                (solve-interval (split-lower interval feature value) result 0)
                (solve-interval (split-upper interval feature value) flow-name (inc i))))
        result (solve-interval interval result 0)))))

(solve-interval [[1 4001] [1 4001] [1 4001] [1 4001]] :in 0)
