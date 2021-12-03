(ns day5
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.set :as s :refer [union]]))

(def input (data/input 2021 5))
(def test-input (slurp "src/day5s.txt"))

(defn parse-int [s] (Integer/parseInt s)) ;;https://stackoverflow.com/a/6197026/5067724
(defn parse-pair [string] (-> string str/trim (str/split #",") (->> (map #(Integer/parseInt %)))))
(defn extract-pairs [[m1 m2]] (let [p1 (parse-pair m1)
                                    p2 (parse-pair m2)]
                                {:xmin (min (first p1) (first p2)) :xmax (max (first p1) (first p2))
                                 :ymin (min (second p1) (second p2)) :ymax (max (second p1) (second p2))}))
(defn update-map [m f]
  (reduce-kv (fn [m k v] 
               (assoc m k (f v))) {} m))


(defn maps [input] (->> input str/split-lines (map #(str/split % #"->")) (map extract-pairs) ))
(defn horizontal? [line] (= (:ymin line) (:ymax line)))
(defn vertical? [line] (= (:xmin line) (:xmax line)))
(defn horizontal->set [line] (for [x (range (:xmin line) (+ 1 (:xmax line)))]
                               {:x x :y (:ymin line)}))
(defn vertical->set [line] (for [y (range (:ymin line) (+ 1 (:ymax line)))]
                             {:x (:xmin line) :y y}))
(defn line->set [line] (cond
                                     (horizontal? line) (horizontal->set line)
                                     (vertical? line) (vertical->set line)))

(def vertical-or-horizontal (filter #(or (horizontal? %) (vertical? %)) maps))
;; horizontal intersects if its y is between
(defn overlap-for [{:keys [max1 min1 max2 min2]}]
  ()
  )
(defn intersection-for [vertical horizontal]
  (let [vertx (:xmin vertical)
        hory (:ymin horizontal)] (when
                                     (and
                                      (<= (:xmin horizontal) vertx (:xmax horizontal))
                                      (<= (:ymin vertical) hory (:ymax vertical)))
                                   (hash-map :x vertx :y hory))))
(defn intersections-for-line [line set]
  (let [potential-intersections (->> set
                                     (filter #(not= line %))
                                     (filter #(if (horizontal? line) (vertical? %) (horizontal? %))))
        check-intersection (if (vertical? line) #(intersection-for line %) #(intersection-for % line))
        ]
    (->> potential-intersections (map check-intersection) (filter some?) first)
    ))

(defn intersections-for-line [line st]
  (let [potential-intersections (->> st
                                     (filter #(not= line %)))
        sets (->> potential-intersections (map line->set) (filter some?) (map set))
        line-pts (->> line line->set set)
        overlapping (fn [ln] (s/intersection ln line-pts))
        ]
    (->> sets (map overlapping) (filter not-empty))
    ;; #break sets
    ))

(intersections-for-line firstline testlines)

(count (let [maps (->> input maps)]
   (->> maps
        (map #(intersections-for-line % maps))
        (filter not-empty)
        flatten
        (apply union)
        )))

(def horizontal-vertical-sets (let [maps (->> test-input
                 maps)
       ]
   (->> maps
        (map line->set)
        (filter some?)
        (map set)
        )))





(def testlines (let [maps (->> test-input
                               maps)
                     ]
                 (->> maps

                      )))

(def firstline (first (let [maps (->> test-input
                                      maps)
                            ]
                        (->> maps

                             ))))


(def testvert (filter vertical? (let [maps (->> test-input
                                                maps)]
                                  (->> maps
                                       ))))











;; parse - 2d representation
;; at least 2 overlap
