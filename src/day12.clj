(ns day12
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]))

(defn input->graph [input]
  (->> input 
        str/split-lines 
        (map #(str/split % #"-"))
        (map #(merge (hash-map (keyword (first %)) (keyword (second %))) (hash-map (keyword (second %)) (keyword (first %)))))
        (apply merge-with vector)
        (map flatten)
        (map #(hash-map (first %) (apply vector (rest %))))
        (into {})
        ))

(defn keyword-lower? [keyw] (every? (fn [^Character x] (Character/isLowerCase x)) (name keyw)))
(defn mcontains? [coll val] (some #(= val %) coll))

(defn dfs
  ([graph start goal]
   (let [visited-nodes #{start}]
     ((dfs graph goal) [start] visited-nodes)))
  ([graph goal]
   (fn search
     [path visited]
     (let [current (peek path)]
       (if (= goal current)
         [path]
         (->> current graph
              (remove visited)
              (mapcat #(search (conj path %) (if (keyword-lower? %) (conj visited %) visited)))))))))

(defn dfs2
  ([graph start goal]
   (let [visited-nodes #{start}]
     ((dfs2 graph goal) [start] visited-nodes false)))
  ([graph goal]
   (fn search
     [path visited dupe-detected]
     (let [current (peek path)
           to-remove (if dupe-detected visited #{:start})]
       (if (= goal current)
         [path]
         (->> current graph
              (remove to-remove)
              (mapcat #(search (conj path %)
                               (if (keyword-lower? %) (conj visited %) visited)
                               (if (or
                                     dupe-detected
                                     (and
                                       (keyword-lower? %)
                                       (mcontains? visited %)))
                                 true
                                 false)))))))))


(let [parsed-input (-> (data/input 2021 12) input->graph)]
  (cl-format true "Part 1: ~d~%" (count (dfs parsed-input :start :end)))
  (cl-format true "Part 2: ~d~%" (count (dfs2 parsed-input :start :end))))
