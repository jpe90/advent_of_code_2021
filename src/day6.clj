(ns day6
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(def input (data/input 2021 6))

(defn group-by-count [fish]
  (into [] (for [i (range 9)]
             (count (filter (partial = i) fish)))))

(defn step [fish]
  (let [new (first fish)
        s (fn [idx _] (cond (or (< idx 6) (= 7 idx))
                            (nth fish (+ 1 idx))
                            (= 6 idx) (+ new (nth fish 7))
                            (= 8 idx) new))]
    (map-indexed s fish)))


(defn step-n [n input] 
  (loop [x n
         state input]
    (if (= 0 x)
      state
      (recur (- x 1) (step state)))))

(defn init [input]
  (as-> input i
    (str/split-lines i)
    (first i)
    (str/split i #",")
    (map #(Integer/parseInt %) i)
    (group-by-count i)))

(defn fish-after [num-days]
  (as-> input i
    (init i)
    (apply + (step-n num-days i))))

;; part 1
(fish-after 80)
;; part 2
(fish-after 256)


