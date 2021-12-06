(ns day3
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.pprint :refer [cl-format]]))

(def input (data/input 2021 3))

(defn bin-string->num-list [bin-string] (map #(. Character getNumericValue %) bin-string))
(defn bin-list->int [lst]  (Integer/parseInt (reduce str lst) 2))
(defn- mk-map [in] {:input-size (count in) :frequency-list (map bin-string->num-list in)})
(defn freq [in] (->> in (str/split-lines) mk-map))
(defn frequency-list [in]
  (update (freq in) :frequency-list (fn [lst] (reduce #(map + %1 %2) lst))))

(defn gamma-rate-list [freq-list]
  (let [most-occuring (fn [num] (if (< (/ (:input-size freq-list) 2) num) 0 1))]
    (map most-occuring (:frequency-list freq-list))))

(defn gamma-decimal [in] (->> in (reduce str) (str "2r") read-string))
(defn epsilon-decimal [in] (->> in bit-not (bit-and 2r111111111111)))

(defn lister [cmp]
  (fn [list] (let [sum (reduce #(map + %1 %2) list)
         cnt (count list)
         avgs (map #(/ % cnt) sum)
         cmpmap (map #(if (cmp (/ 1 2) %) 1 0) avgs)]
     cmpmap)))

(defn freq-info [in] (->> in (str/split-lines) (map bin-string->num-list)))

(defn filter-bin-list [list f]
  (loop [ind 0
         remaining-list list]
    (let [num (f remaining-list)
          digit (nth num ind)
          filtered-list (filter #(= (nth % ind) digit) remaining-list)]
      (if (> (count filtered-list) 1)
        (recur (inc ind) filtered-list)
        (first filtered-list)))))

(defn solve [lst]
  (let [i (filter-bin-list lst (lister <=))
        j (filter-bin-list lst (lister >))
        a (bin-list->int i)
        b (bin-list->int j)]
    (* a b)))

;; part 1
(let [test-input-frequency (frequency-list input)
      gamma-list (gamma-rate-list test-input-frequency)
      gamma (gamma-decimal gamma-list)
      epsilon (epsilon-decimal gamma)]
  (* gamma epsilon))
;; part 2
(solve (freq-info input))







