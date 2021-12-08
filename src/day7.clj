(ns day7
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.pprint :refer [cl-format]]))

(defn parse-input [input]
  (as-> input i
    (str/trim i) (str/split i #",") (map #(Integer/parseInt %) i)))

(defn sum-of-distances
  ([number in] (->> in (map #(abs (- % number))) (apply +)))
  ([number in f] (->> in (map #(abs (- % number))) (map f) (apply +))))

(def calculate-fuel (memoize (fn [d] (apply + (take d (iterate inc 1))))))

(defn solve [f input]
  (as-> (range (apply min input) (apply max input)) i
    (map #(f % input) i) (apply min i)))

(let [parsed-input (-> (data/input 2021 7) parse-input)]
  (cl-format true "Part 1: ~d~%" (solve sum-of-distances parsed-input))
  (cl-format true "Part 2: ~d~%" (solve sum-of-distances-2 parsed-input)))
