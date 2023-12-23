(ns adamharbin.advent.2023.22
  (:require [clojure.string :as s]
            [clojure.set :as cset]
            [adamharbin.advent.common :refer [zip]]))

;; Parse Input

(defn parse-line [input]
  (let [[_ x1 y1 z1 x2 y2 z2] (re-matches #"(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)" input)
        [x1 y1 z1 x2 y2 z2] (mapv parse-long [x1 y1 z1 x2 y2 z2])]
    (assert (and (<= x1 x2) (<= y1 y2) (<= z1 z2))
            (format "Dimensions not increasing for %s." input))
    {:x [x1 x2]
     :y [y1 y2]
     :z [z1 z2]}))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(def raw-input (slurp "input.dat"))
(def parsed-input (parse-input raw-input))

;; Part 1

(defn find-intersection [brickA brickB dimension]
  "Given a dimention keyword (:x :y or :z) determine if the bricks occupy
  the same space in that dimension."
  (let [[a-lo a-hi] (dimension brickA)
        [b-lo b-hi] (dimension brickB)]
    (cond
      (<= a-lo b-lo a-hi b-hi) [b-lo a-hi]
      (<= b-lo a-lo b-hi a-hi) [a-lo b-hi]
      (<= a-lo b-lo b-hi a-hi) [b-lo b-hi]
      (<= b-lo a-lo a-hi b-hi) [a-lo a-hi]
      :else nil)))

(defn collide? [brick1 brick2]
  "Determine if 2 bricks occupy the same space."
  (let [x-intersection (find-intersection brick1 brick2 :x)
        y-intersection (find-intersection brick1 brick2 :y)
        z-intersection (find-intersection brick1 brick2 :z)]
    (and (some? x-intersection)
         (some? y-intersection)
         (some? z-intersection))))

(defn on-ground? [brick]
  "Determine if a brick is colliding with the ground"
  (<= (apply min (:z brick)) 0))

(defn drop-brick [brick]
  "Calculate the position of a brick after dropping 1 unit in z-direction."
  (let [z-coords (:z brick)]
    (assoc brick :z (mapv dec z-coords))))

(defn drop-until-stops [brick other-bricks]
  "Calculate position of a brick that drops until it hits the ground or another brick."
  (let [dropped-once (drop-brick brick)
        on-ground (on-ground? dropped-once)
        hits-another-brick (some #(collide? dropped-once %) other-bricks)]
    (if (or on-ground hits-another-brick)
      brick
      (recur dropped-once other-bricks))))

(defn settle-bricks [bricks]
  "Compute final resting position of a collection of bricks after dropping all of them
   until they stop."
  (let [sorted-by-z (sort-by #(apply min (:z %)) bricks)]
      (loop [xs sorted-by-z
             dropped #{}]
        (if (seq xs)
          (let [b (first xs)
                new-b (drop-until-stops b (into dropped (rest xs)))]
            (recur (rest xs) (conj dropped new-b)))
          dropped))))

(defn find-rests-on [brick bricks]
  "Finds set of bricks this brick rests on."
  (let [other-bricks (filter #(not= % brick) bricks)
        dropped-brick (drop-brick brick)]
    (filter #(collide? % dropped-brick) other-bricks)))

(defn assign-ids [bricks]
  "Tag bricks with an integer id to distinguish them."
  (->> (zip (range (count bricks)) bricks)
       (map (fn [[id m]] (assoc m :id id)))))

(defn solve [bricks]
  "Calculate set of bricks which are not the only brick that bricks above rest on."
  (let [settled-bricks (settle-bricks (assign-ids bricks))]
    (->> settled-bricks ; compute final position of bricks after falling
         (map #(find-rests-on % settled-bricks)) ; find all sets of 'rested on' bricks
         (filter #(= 1 (count %))) ; only consider bricks which 'rest on' one other
         (apply concat) ; merge to one sequence
         (into #{}) ; convert to set
         (cset/difference (into #{} settled-bricks)) ; all bricks not in computed set
         count)))

(solve parsed-input)

;; Part 2

(defn solve-2 [bricks]
  "For each brick, recalculate the final resting position of all other bricks
   when it is removed. For each brick id, find whether it changed position on
   removal and accumulate the total number bricks moved this way."
  (let [settled-bricks (settle-bricks (assign-ids bricks))]
    (loop [xs settled-bricks
           total-score 0]
      (if (seq xs)
        (let [b (first xs)
              other (filter #(not= b %) settled-bricks)
              after-falling (settle-bricks other)
              old-locations (cset/index other [:id])
              new-locations (cset/index after-falling [:id])
              moved (filter #(not= (old-locations %) (new-locations %))
                            (keys old-locations))]
          (recur (rest xs) (+ total-score (count moved))))
        total-score))))

(solve-2 parsed-input)
