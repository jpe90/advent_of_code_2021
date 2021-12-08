(ns day9
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.pprint :refer [cl-format]]))

(defn parse-input [input]
  (as-> input i
    (str/split-lines i) (hash-map :line-length (->> i first count)
                                  :char-seq (->> i (apply concat) (map #(Character/digit % 10))))))

(defn left-edge? [idx line-length] (or (= 0 idx) (= 0 (mod idx line-length))))
(defn right-edge? [idx line-length] (and (not= 0 idx) (= (- line-length 1) (mod idx line-length))))

(defn validate-edges [val idx line-length] (cond
                                             (right-edge? idx line-length) (not (left-edge? val line-length)) ;;right
                                             (left-edge? idx line-length) (not (right-edge? val line-length)) ;;left
                                             :else true))

(defn neighbor-indices [idx line-length total-count] (filter
                                                      #(and (>= % 0)
                                                            (< % total-count)
                                                            (validate-edges % idx line-length))
                                                      [(+ idx 1) (- idx 1) (- idx line-length) (+ idx line-length)]))

(def get-neighbors (memoize (fn [input idx]
                      (map #(nth (:char-seq input) %) (neighbor-indices idx (:line-length input)
                                                                        (count (:char-seq input)))))))


(def bigger-neighbors-idx (memoize (fn [input idx] 
                                     (filter #(< (nth (:char-seq input) idx) (nth (:char-seq input) %))
                                             (neighbor-indices idx (:line-length input) (count (:char-seq input)))))))

(def adjacent-basin-indices (memoize (fn [input idx] (filter #(not= 9 (nth (:char-seq input) %))
                                                             (bigger-neighbors-idx input idx)))))

(defn ->basin-seq [input index]
  (let [f (adjacent-basin-indices input index)
        g (flatten (concat (map #(->basin-seq input %) f)))]
    (cons index g)))

(defn minimum-of? [val coll] (every? #(< val %) coll))
(defn minimum-of-neighbors? [input idx] (minimum-of? (nth (:char-seq input) idx) (get-neighbors input idx)))
(defn local-minimums [input] (map-indexed (fn [idx item] (hash-map :idx idx
                                                                   :val item
                                                                   :minimum (minimum-of-neighbors? input idx)))
                                          (:char-seq input)))

(defn sum-local-minimums [input] (->> input
                                      local-minimums
                                      (filter #(true? (:minimum %)))
                                      (map :val)
                                      (map (partial + 1))
                                      (apply +)))

(defn product-largest-basins [input] (->> input
                                          local-minimums
                                          (filter #(true? (:minimum %)))
                                          (map :idx)
                                          (map #(count (distinct (->basin-seq input %))))
                                          sort
                                          reverse
                                          (take 3)
                                          (apply *)))

(let [parsed-input (-> (data/input 2021 9) parse-input)]
  (cl-format true "Part 1: ~d~%" (sum-local-minimums parsed-input))
  (cl-format true "Part 2: ~d~%" (product-largest-basins parsed-input)))