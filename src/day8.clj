(ns day8
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.pprint :refer [cl-format]]
            [clojure.math.combinatorics :refer [permutations]]))

(def input (data/input 2021 8))
(def test-input (slurp "src/8.txt"))
(def test-input-alt (slurp "src/8a.txt"))

(defn parse-input [input]
  (as-> input i
    (str/split-lines i)
    (map #(str/split % #"\|") i)
    (map (fn [line] (map #(str/trim %) line)) i)))

(defn part1 [parsed-input]
  (->> parsed-input
       (map #(-> % second (str/split #"\s+")))
       flatten
       (filter #(or (= (count %) 2)
                    (= (count %) 3)
                    (= (count %) 4)
                    (= (count %) 7)))
       count))


(def segments ["abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"])
(defn segment-permutations [permutations] (->> permutations (map vector "abcdefg") (into {})))
(def decoder-seq (map segment-permutations (permutations "abcdefg")))
(defn decode [word decoder] (map decoder word))
(defn dec [word] (->> (map (nth decoder-seq 22) word) (apply str)))
(defn decode-segments [decoder] (map (fn [word] (->> (map decoder word) (apply str))) segments))
(def inputs (->> test-input parse-input (map first) first))
(def wtf (str/split (->> test-input parse-input (map first) first) #"\s+"))


(print (decode-segments (first decoder-seq)))


;; (defn )

                                        ;(= (sort '(1 2 3)) (sort [2 3 1]))
                                        ;(map (decode-segments (first decoder-seq)) segments)
                                        ;(rest decoder-seq)
                                        ;(= (sort (decode-segments (nth decoder-seq 2))) (sort wtf))
                                        ;(sort segments)


(defn similar? [fst snd] (and (= (count fst) (count snd)) (every? (set snd) fst)))

(defn compare [fst snd] (map-indexed (fn [idx elem] (similar? elem (nth snd idx))) fst))


(similar? [\c \b \a] [\a \d \b])

(defn decode [scrambled]
  (loop [permutation decoder-seq]
    (let [f (sort (decode-segments (first permutation)))
          g (sort scrambled)]
      (if (not-empty (filter true? (compare f g)))
        (first permutation)
        (recur (rest permutation))))))

(loop [permutation decoder-seq]
  (let [f (sort (decode-segments (first permutation)))
        g (sort wtf)]
    (if (compare f g)
      (first permutation)
      (recur (rest permutation)))))

(let [f (sort (decode-segments (first decoder-seq)))
      g (sort wtf)]
  (list f g))

(loop [permutation decoder-seq]
  (let [f (sort (decode-segments (first permutation)))
        g (sort wtf)]
    (if (similar? f g)
      "W T F" (recur (rest permutation)))))



(sort (map dec segments))
(decode "abcd" (nth decoder-seq 22))

                                        ;(defn part2 [parsed-input]
                                        ;  (let [
                                        ;        decode-segment (fn [decoder] (fn [segment] (map decoder segment)))
                                        ;        awaiting-segment (map (fn [segment] ((map (fn [decoder]
                                        ;                                                    (decode-segment decoder)) decoder-seq) segment) segments))]
                                        ;    ;; (map (awaiting-segment) segments)
                                        ;    decoder-seq
                                        ;    ))
                                        ;
                                        ;(map vector "abc" "def")
                                        ;
                                        ;(def tst (->> test-input parse-input part2))
                                        ;
                                        ;
                                        ;
                                        ;(def maps (->> test-input parse-input part2)
                                        ;  )
                                        ;(def seconds (second maps))
                                        ;()


                                        ;
                                        ;
                                        ;
                                        ;(comment (let [parsed-input (-> test-input parse-input)]
                                        ;   (cl-format true "Part 1: ~d~%" parsed-input)
                                        ;   ;; (cl-format true "Part 2: ~d~%" parsed-input)
                                        ;   ))


