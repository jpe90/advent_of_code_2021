(ns day2
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(def input
  (->> (data/input 2021 2)
       (str/split-lines)
       (map #(str/split % #" "))
       (map (fn [[fst snd]] {:direction (keyword fst) :magnitude (Integer/parseInt snd)}))))

(defn part-1 [acc {:keys [direction magnitude]}]
  (case direction
    :forward  (update acc :horizontal + magnitude)
    :up   (update acc :depth - magnitude) 
    :down (update acc :depth + magnitude)))

(defn part-2 [acc {:keys [direction magnitude]}]
  (case direction
    :forward (-> acc (update :horizontal + magnitude) (update :depth + (* (:aim acc) magnitude)))
    :up   (update acc :aim - magnitude) 
    :down (update acc :aim + magnitude)))


(defn solve [reducer input] (let [init {:horizontal 0 :depth 0 :aim 0}
                                  map-vals (reduce reducer init input)]
                              (* (:horizontal map-vals) (:depth map-vals))))

(comment
  (solve part-1 input)
  (solve part-2 input))
