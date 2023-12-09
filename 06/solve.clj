(ns adamharbin.advent.2023.6)

;; Part 1

(def input [[59 597]
            [79 1234]
            [65 1032]
            [75 1328]])

(def sample [[7 9]
             [15 40]
             [30 200]])

(defn dist [wait length]
  (* wait (- length wait)))

(defn num-ways-to-win [[length goal]]
  (count (filter #(< goal (dist % length)) (range 1 length))))

(defn solve [input]
  (->> input
    (map num-ways-to-win)
    (apply *)))

(solve input)

;; Part 2

;; Dist function above is a parabola that intersects the line y=goal at 2 points.
;; Use the quadratic formula to find these intersections. L = length, G = goal.

;; Dist Function
;; G = x * (L - x)
;; 0 = x * (L - x) - G
;; 0 = Lx - x^2 - G
;; 0 = - x^2 + Lx - G

;; Inputs to Formula
;; A = -1
;; B = L
;; C = -G

;;    x = (-L + sqrt(L^2 - 4G)) / -2
;; or x = (-L - sqrt(L^2 - 4G)) / -2

(def L 59796575)
(def G 597123410321328)

(def solution1
  (/ (+ (- L) (int (Math/sqrt (- (* L L) (* 4 G)))))
   -2))

(def solution2
  (/ (- (- L) (int (Math/sqrt (- (* L L) (* 4 G)))))
   -2))

(def int-solution-1 (int (Math/ceil solution1)))
(def int-solution-2 (int (Math/floor solution2)))

(inc (- int-solution-2 int-solution-1))
