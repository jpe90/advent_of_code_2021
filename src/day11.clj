(ns day11
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]
            [clojure.set :refer [intersection]]))

(defn input->board [input]
  (->> input
       str/split-lines
       (mapv (fn [line] (mapv (comp #(Long/parseLong %) str) line)))
       (map-indexed (fn [y xline] (map-indexed (fn [x v] [[x y] v]) xline)))
       (apply concat)
       (into {})))

(defn flash-cell [[k v] flash-points]
  (let [adjacent-offsets [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
        neighbors (fn [pt] (mapv (fn [[a b]] (vector (+ a (first pt)) (+ b (second pt)))) adjacent-offsets))
        new-value (+ v (count (intersection flash-points (set (neighbors k)))))]
    [k (if (> new-value 9) 0 new-value)]))

(defn flash-board
  ([grid] (flash-board grid (set (map first (filter #(> (second %) 9) grid)))))
  ([grid flash-points]
   (let [new-grid (into {} (map #(flash-cell % flash-points) grid))
         new-flash-points (set (keys (filter #(zero? (second %)) new-grid)))]
     (if (= flash-points new-flash-points) new-grid
                                           (recur grid new-flash-points)))))

(defn step [board] (flash-board (reduce-kv #(assoc %1 %2 (inc %3)) {} board)))

(defn part1 [input] (->> (input->board input)
      (iterate step)
      (take (inc 100))
      (rest)
      (mapcat #(filter zero? (vals %)))
      (count)))

(defn part2 [input] (->> (input->board input)
      (iterate step)
      (take-while #(not (every? zero? (vals %))))
      (count)))

(let [input (data/input 2021 11)]
  (cl-format true "Part 1: ~d~%" (part1 input))
  (cl-format true "Part 2: ~d~%" (part2 input)))