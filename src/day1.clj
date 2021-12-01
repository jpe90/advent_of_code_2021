(ns day1
  (:require [aocd.core :as data]
            [clojure.string :as cstr]))

(def input (data/input 2021 1))

(def part-1
  (->> input
       (cstr/split-lines)
       (map #(Integer/parseInt %))
       (partition 2 1)
       (filter #(< (first %) (second %)))
       count))

(def part-2
  (->> input
       (cstr/split-lines)
       (map #(Integer/parseInt %))
       (partition 3 1)
       (map #(reduce + %))
       (partition 2 1)
       (filter #(< (first %) (second %)))
       count))


