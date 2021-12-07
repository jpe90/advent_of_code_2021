(ns day7
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.math.numeric-tower :refer [abs]]))

(def input (data/input 2021 7))

(defn parse-input [input]
  (as-> input i
        (str/trim i)
        (str/split i #",")
        (map #(Integer/parseInt %) i)))

(defn distance-from [alignment]
  (fn [potential-position]
    (abs (- alignment potential-position))))

(def parsed-input (parse-input input))

(defn input-range-for [input]
  (range (apply min input) (apply max input)))

(defn calculate-fuel [distance]
  (apply + (take distance (iterate inc 1))))

(defn sum-of-distances [number in]
  (->> in
       (map (distance-from number))
       (apply +)))

(defn sum-of-distances-2 [number in]
  (->> in
       (map (distance-from number))
       (map calculate-fuel)
       (apply +)))

(defn solve [f input]
  (as-> (input-range-for input) i
        (map #(f % input) i)
        (apply min i)))

(let [parsed-input (parse-input input)]
  (solve sum-of-distances parsed-input)
  (solve sum-of-distances-2 parsed-input))
