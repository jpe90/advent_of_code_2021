(ns day13
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]))

(def test-input (slurp "src/13.txt"))

(defn parse-point [line] (let [vals (map #(Long/parseLong %) (str/split line #","))]
                           {:x (first vals) :y (second vals)}))
(defn parse-instruction [line]
  (let [res (str/split line #"=")
        direction (keyword (last (str/split (first res) #"\s+")))
        value (Long/parseLong (second res))]
    {direction value}))
(defn delegate-line [line]
  (cond
    (str/includes? line ",") (parse-point line)
    (str/includes? line "=") (parse-instruction line)
    :else nil))
(defn points->points-info [pts] {:points pts :ymax (apply max (map :y pts)) :xmax (apply max (map :x pts))})
(defn parse-input [input]
  (as-> input i
    (str/split-lines i)
    (mapv delegate-line i)
    (split-with some? i)
    (merge (points->points-info(first i)) {:instructions (filter some? (second i))})
    ))

;; if line has a comma, add to points
;; if it has an equal, add to instructions
(def test (parse-input test-input))
(def real (parse-input (data/input 2021 7)))
(:points test)
                                        ;
(defn update-y-point [y ymax] (if (> y ymax) (- y ymax) y))
(update-y-point 4 3)

;; not intuitive what's going on here 
(defn update-y-pts [oldmax newmax]
  (fn [current] (map #(hash-map :x (:x %)
                   :y (if (> (:y %) newmax)
                         (- oldmax (:y %))
                        (:y %))) current)))

(defn fold-y [pts-info]
  (let [newmax (quot (:ymax pts-info) 2)] (as-> test i
       (assoc i :ymax newmax)
       ;((fn [current] (update current :points (fn [pts] (map
       ;                                                   #(if (> (:y %) (:ymax current))
       ;                                                      {:x (:x %) :y (- (:y %) (:ymax current))}
                                        ;                                                      99)) pts))))
       ;; trying to update a seq nested in a hashmap by mapping over it with a value
        (update i :points (update-y-pts (:ymax pts-info) newmax))
        )))
inc

;(map #(hash-map :x (:x %) :y (if (> 2 (:y %)) 42 (:y %))) '({:x 6, :y 10}
;            {:x 0, :y 14}
;            {:x 9, :y 10}
;            {:x 0, :y 3}
;            {:x 10, :y 4}
;            {:x 4, :y 11}
;            {:x 6, :y 0}
;            {:x 6, :y 12}
;            {:x 4, :y 1}
;            {:x 0, :y 13}
;            {:x 10, :y 12}
;            {:x 3, :y 4}
;            {:x 3, :y 0}
;            {:x 8, :y 4}
;            {:x 1, :y 10}
;            {:x 2, :y 14}
;            {:x 8, :y 10}
;            {:x 9, :y 0}))
(fold-y test)
;(defn fold [points ins]
;  (if (some? (:x ins)) :x :y)
;
;  )
;
;(fold "e" {:x "b"})




;; => ["6,10" "0,14" "9,10" "0,3" "10,4" "4,11" "6,0" "6,12" "4,1" "0,13" "10,12" "3,4" "3,0" "8,4" "1,10" "2,14" "8,10" "9,0" "" "fold along y=7" "fold along x=5"]


;; defn for fold y - takes a list of points and a y pivot
;; map all y coords of points above pivot to (val - pivot)
;; distinct on that

;; same for fold x- 

;; (let [parsed-input (-> (data/input 2021 7) parse-input)]
;;   (cl-format true "Part 1: ~d~%" )
;;   (cl-format true "Part 2: ~d~%" ))

