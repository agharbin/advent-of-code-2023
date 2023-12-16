(ns adamharbin.advent.2023.15
  (:require [clojure.string :as s]
            [adamharbin.advent.common :refer [zip]]))

;; Input Parsing

(defn parse-input [input]
  (-> input
      s/trim
      (s/split #",")))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))

;; Part 1

(defn compute-hash
  ([input] (compute-hash input 0))
  ([input start-val]
   (if (seq input)
     (recur
       (rest input)
       (-> (first input)
            int
            (+ start-val)
            (* 17)
            (mod 256)))
     start-val)))

(apply + (map compute-hash parsed-input))

;; Part 2

(defn parse-step [step-str]
  "Parses e.g. 'rc=1' to [:insert 'rc' 1] and 'bc-' to [:remove 'b']."
  (if (= \- (last step-str))
    [:remove (apply str (drop-last 1 step-str))]
    (let [[_ k v] (re-matches #"(.*)=(.*)" step-str)]
      [:insert k (parse-long v)])))

(defn replace-value-in-box [box k v]
  "Return the vector `box` with the value [k v] substituted for the pair with matching k."
  (mapv (fn [[k' v']] (if (= k k') [k v] [k' v'])) box))

(defn matches [k]
  "Generate predicate checking if key-value pair has key matching k."
  (fn [[k' _]] (= k k')))

(defn insert-box [boxes [k v]]
  "Inserts a key-value pair into the box dictionary, replacing the value if the key is already present."
  (let [box-num (compute-hash k)
        box (get boxes box-num)]
    (if (nil? box)
      (assoc boxes box-num [[k v]])
      (if (some (matches k) box)
        (assoc boxes box-num (replace-value-in-box box k v))
        (assoc boxes box-num (conj box [k v]))))))

(defn remove-box [boxes k]
  "Returns the box dictionary with the key-value pair matching key `k` removed from the appropriate box."
  (let [box-num (compute-hash k)
        box (get boxes box-num)]
    (if (nil? box)
      boxes
      (assoc boxes box-num (->> box (remove (matches k)) vec)))))

(defn process-step [boxes [ins k v]]
  (case ins
    :insert (insert-box boxes [k v])
    :remove (remove-box boxes k)))

(defn compute-box-score [[box-number box]]
  (apply +
    (for [[i [k v]] (zip (range 1 (-> box count inc)) box)]
      (* (inc box-number) i v))))

(defn compute-score [boxes]
  (->> boxes
       (map compute-box-score)
       (apply +)))

(->> parsed-input
     (map parse-step)
     (reduce process-step {})
     compute-score)
