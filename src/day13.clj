(ns day13
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]))

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
    (merge (points->points-info(first i)) {:instructions (filter some? (second i))})))

(defn update-x-pts [newmax]
  (fn [current] (distinct (map #(hash-map :x (if (> (:x %) newmax)
                                               (- newmax (- (:x %) newmax ))
                                               (:x %))
                                          :y (:y %)) current))))

(defn update-y-pts [newmax]
  (fn [current] (distinct (map #(hash-map :x (:x %)
                                          :y (if (> (:y %) newmax)
                                               (- newmax (- (:y %) newmax ))
                                               (:y %))) current))))

(defn fold-y [pts-info fy]
  (as-> pts-info i
    (assoc i :ymax fy)
    (update i :points (update-y-pts fy))
    (update i :instructions rest)))

(defn fold-x [pts-info fx]
  (as-> pts-info i
    (assoc i :xmax fx)
    (update i :points (update-x-pts fx))
    (update i :instructions rest)))

(defn fold [in]
  (let [ins (first (:instructions in))]
    (cond
      (some? (:x ins)) (fold-x in(:x ins))
      (some? (:y ins)) (fold-y in (:y ins))
      :else in)))

(defn fold-folds [in]
  (loop [x in]
    (let [ins (first (:instructions x))]
      (cond
        (some? (:x ins)) (recur (fold-x x (:x ins)))
        (some? (:y ins)) (recur (fold-y x (:y ins)))
        :else x))))

(defn mcontains? [coll val] (some #(= val %) coll))
(defn prn [in]
  (for [y (range (:ymax in))]
    (for [x (range (:xmax in))]
      (let [prnval (if (mcontains?  (:points in) {:x x :y y}) "#" ".")]
        (if (= x (- (:xmax in) 1))
          (print (str prnval "\n"))
          (print prnval))))))

 (let [parsed-input (-> (data/input 2021 13) parse-input)]
   (cl-format true "Part 1: ~d~%" (count (:points (fold parsed-input))))
   (cl-format true "Part 2: ~%" )
   (prn (fold-folds parsed-input)))