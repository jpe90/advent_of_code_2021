(ns day10
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]
            [clojure.set :refer [map-invert]]))

(defn parse-input [input]
  (as-> input i
    (str/split-lines i)))

(defn get-unbalanced [line]
  (try
    (let [chunks {\[ \] \( \) \{ \} \< \>}
          process-chunks (fn [incomplete chr]
                 (cond
                   (chunks chr) (conj incomplete chr)
                   ((map-invert chunks) chr)
                   (if (= (chunks (peek incomplete)) chr)
                     (pop incomplete)
                     (throw
                       (ex-info
                         "Unmatched closing chunk" {:invalid-chunk chr})))))]
      (reduce process-chunks [] line))
    (catch Exception e
      (ex-data e))))

(defn part1 [input] (->> input
                         (map get-unbalanced)
                         (filter #(some? (:invalid-chunk %)))
                         (map  #({\) 3 \} 1197 \] 57 \> 25137} (:invalid-chunk %)))
                         (apply +)))

(defn calculate-score [chars]
  (->> chars reverse (map #({\( 1 \[ 2 \{ 3 \< 4} %)) (reduce #(+ (* 5 %1) %2) 0)))

(defn part2 [input] (let [scores (->> input
                                      (map get-unbalanced)
                                      (filter #(nil? (:invalid-chunk %)))
                                      (map calculate-score)
                                      sort)]
                      (nth scores (quot (count scores) 2))))

(let [parsed-input (-> (data/input 2021 10) parse-input)]
  (cl-format true "Part 1: ~d~%" (part1 parsed-input))
  (cl-format true "Part 2: ~d~%" (part2 parsed-input)))