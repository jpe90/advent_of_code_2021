(ns day8
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.pprint :refer [cl-format]]))

(def input (data/input 2021 8))
(def test-input (slurp "src/8.txt"))
(defn parse-input [input]
  (as-> input i
    (str/split-lines i)
    (map #(str/split % #"\|") i)
    (map (fn [line] (map #(str/trim %) line) ) i)
    (map #(second %) i)
    (map #(str/split % #"\s+") i)
    (flatten i)
    (filter #(or (= (count %) 2)
                 (= (count %) 3)
                 (= (count %) 4)
                 (= (count %) 7)) i)
    (count i)
    ;; (map count i)
    ;; (distinct i)

    ;; (map count i)
    ;; (map (fn [line] (map #(count %) line) ) i)
    ;; (map str/trim i)
    ))

(-> input parse-input)

(comment (let [parsed-input (-> test-input parse-input)]
   (cl-format true "Part 1: ~d~%" parsed-input)
   ;; (cl-format true "Part 2: ~d~%" parsed-input)
   ))


