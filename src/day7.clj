(ns day7
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.math.numeric-tower :refer [abs]]))

(def input (data/input 2021 7))

(def test-input (slurp "src/7.txt"))


(defn parse-input [input]
  (as-> input i
    (str/trim i)
    (str/split i #",")
    (map #(Integer/parseInt %) i)
    ))

(defn distance-from [alignment]
  (fn [potential-position]
    (abs (- alignment potential-position))))
(defn input-range-for [input]
  (range (apply min input) (apply max input)))


(def parsed-test-input (parse-input test-input))
(def parsed-input (parse-input input))

(apply + (take 4 (iterate inc 1)))
((distance-from 5) 1)

(defn calculate-fuel [distance]
  (apply + (take distance (iterate inc 1))))



(defn sum-of-distances [number]
  (->> parsed-input
       (map (distance-from number))
       (apply +)
       ))

(defn sum-of-distances-2 [number]
  (->> parsed-input
       (map (distance-from number))
       (map calculate-fuel)
       (apply +)
       ))

(sum-of-distances-2 5)
(map #(sum-of-distances-2 %) parsed-test-input)

(defn part2 [input]
  (as-> input i
    (map #(sum-of-distances-2 %) i)
    (apply min i)
    ))

(part2 parsed-test-input)

(defn part1 [input]
  (as-> input i
    (map #(sum-of-distances %) i)
    (apply min i)
    ))

(defn part2 [input]
  (as-> (input-range-for input) i
    (map #(sum-of-distances-2 %) i)
    (apply min i)
    ))

(part1 parsed-input)
(part2 parsed-input)


(input-range-for parsed-test-input)


;; (map distance-fns)



parsed-input

(parse-input test-input)

