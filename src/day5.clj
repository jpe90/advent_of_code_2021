(ns day5
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.set :refer [intersection union]]
            [clojure.math.numeric-tower :refer [abs]]))

(def input (data/input 2021 5))
(defn horizontal? [line] (= (:y1 line) (:y2 line)))
(defn vertical? [line] (= (:x1 line) (:x2 line)))
(defn parse-pair [string] (-> string str/trim (str/split #",") (->> (map #(Integer/parseInt %)))))
(defn pairs->line [[m1 m2]] (let [p1 (parse-pair m1)
                                    p2 (parse-pair m2)]
                                {:x1 (first p1) :y1 (second p1) :x2 (first p2) :y2 (second p2)}))
(defn parse-lines [input] (->> input str/split-lines (map #(str/split % #"->")) (map pairs->line)))
(defn line->points [line]
  (let [xd (- (:x2 line) (:x1 line))
        yd (- (:y2 line) (:y1 line))
        maxd (max (abs xd) (abs yd))
        dx (/ xd maxd)
        dy (/ yd maxd)]
    (for [step (range (inc maxd))]
      {:x (+ (:x1 line) (* step dx)) :y (+ (:y1 line) (* step dy))})))

(defn intersections-for-line [line st]
  (let [potential-intersections (->> st
                                     (filter #(not= line %)))
        sets (->> potential-intersections (map line->points) (filter some?) (map set))
        line-pts (->> line line->points set)
        overlapping (fn [ln] (intersection ln line-pts))
        ]
    (->> sets (map overlapping) (filter not-empty))))

(defn count-intersections [lines]
  (->> lines
       (map #(intersections-for-line % lines))
       (filter not-empty)
       flatten
       (apply union)
       count
       ))

(defn vertical-horizontal-intersections [input]
  (let [lines (->> input parse-lines (filter #(or (horizontal? %) (vertical? %))))]
    (count-intersections lines)))

(defn all-intersections [input]
  (let [lines (->> input parse-lines)]
           (count-intersections lines)))

;;part 1
(vertical-horizontal-intersections input)
;;part 2
(all-intersections input)