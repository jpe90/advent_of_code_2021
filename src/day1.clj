(ns day1
  (:require [aocd.core :as data]
            [clojure.string :as cstr]))

(def input (data/input 2021 1))
(def numeric-input (->> input (cstr/split-lines) (map #(Integer/parseInt %))))

(defn part-1 [in]
  (->> in
       (partition 2 1)
       (filter #(< (first %) (second %)))
       count))

(defn part-2 [in]
  (->> in
       (partition 3 1)
       (map #(reduce + %))
       (part-1)))

