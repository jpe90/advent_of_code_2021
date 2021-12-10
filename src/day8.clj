(ns day8
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.set :refer [map-invert]]
            [clojure.pprint :refer [cl-format]]
            [clojure.math.combinatorics :refer [permutations]]
            [clojure.math.numeric-tower :refer [expt]]))

(def input (data/input 2021 8))

(defn parse-input [input]
  (as-> input i
    (str/split-lines i)
    (map #(str/split % #"\|") i)
    (map (fn [line] (map #(str/trim %) line)) i)
    (map (fn [line] (map #(str/split % #"\s+") line)) i)))

(defn part1 [parsed-input]
  (->> parsed-input
       (map #(second %))
       flatten
       (filter #(or (= (count %) 2)
                    (= (count %) 3)
                    (= (count %) 4)
                    (= (count %) 7)))
       count))

(defn sort-string [string]
  (->> string char-array chars seq sort (apply str)))

(def segments (map-indexed #(vector %1 %2) ["abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"]))

(defn segment-permutations [permutations] (->> permutations (map vector "abcdefg") (into {})))
(def decoder-seq (map segment-permutations (permutations "abcdefg")))
(defn decode-string [decoder string] (->> (map decoder string) (apply str)))
(defn decode-strings [decoder strings] (map #(decode-string decoder %) strings))

(defn cmp [fst snd] (map-indexed (fn [idx elem] (= elem (nth snd idx))) fst))
(defn sort-string-seq [sq] (sort (map sort-string sq)))
(defn decode-and-sort [decoder-seq] (->> decoder-seq (map #(decode-strings % (map second segments))) (map sort-string-seq)))
(defn get-decoder [line]
  (loop [ds decoder-seq]
    (let [candidate (first (decode-and-sort ds))]
      (if (every? true? (cmp candidate line))
        (first ds)
        (recur (rest ds))))))

(defn line-decoder [decode-map]
  (->> segments
       (into {} (map #(map-invert (hash-map (first %) (apply str (sort (decode-string decode-map (second %))))))))))
(defn seqmap [mp sq] (map #(get  mp %) sq))
(defn smush-numbers [numbers] (map-indexed (fn [idx item] (* item (expt 10 idx))) (reverse numbers)))

(defn part2 [parsed-input]
  (->> parsed-input
       (map #(vector (line-decoder (get-decoder (sort-string-seq (first %)))) (map sort-string (second %))))
       ;; given a seq and a map, map the
       (map #(seqmap (first %) (second %)))
       (map #(apply + (smush-numbers %)))
       (apply +)))

(let [parsed-input (-> (data/input 2021 8) parse-input)]
  (cl-format true "Part 1: ~d~%" (part1 parsed-input))
  (cl-format true "Part 2: ~d~%" (part2 parsed-input)))
