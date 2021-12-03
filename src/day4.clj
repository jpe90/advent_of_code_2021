(ns day4
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(def input (data/input 2021 4))

(defn extract-board [elem] (->> elem (map #(str/split % #"\s+")) flatten (filter not-empty)))

(defn parse-input [input] {:drawn-numbers (str/split (first input) #",")
                           :game-boards   (->> input
                                               rest
                                               (filter not-empty)
                                               (partition 5)
                                               (map extract-board))})
(defn run [in] (->> in (str/split-lines) parse-input))

(defn horizontals [board]
  (for [rows (range 5)]
    (let [sub-board (drop (* 5 rows) board)]
      (take 5 sub-board))))
(defn verticals [board]
  (for [rows (range 5)]
    (for [cols (range 5)]
      (nth board (+ (* cols 5) rows)))))

(defn contains-elem? [item coll] (not= (.indexOf coll item) -1))
(defn contains-coll [little big] (every? #(contains-elem? % little) big))

(defn check-winx [board called]
  (let [to-check (->> (concat (horizontals board) (verticals board)))
        winners (filter #(contains-coll called %) to-check)]
    (if (not-empty winners)
      board
      nil))
  )

(defn win-state [board numbers] (loop [x 1]
                 (let [drawn-numbers (take x numbers)
                       win-state-reached? (some? (check-winx board drawn-numbers))]
                   (if win-state-reached?
                     {:drawn drawn-numbers :winning-board board}
                     (recur (inc x))))))

(defn loser [boards numbers] (last (sort-by :lendrawn (map #(win-state (list %) numbers) boards))))
(defn loservals [boards] (first (:winning-board (loser boards))))
(defn loserdrawn [boards] (:drawn (loser boards)))
(defn last-winning-board [boards]
  (->> boards
       (map #(win-state % numbers))
       (sort-by #(count (:drawn %)))

(let [run-info (run input)
      boards (:game-boards run-info)
      numbers (:drawn-numbers run-info)]
  (comment (*
            (Integer/parseInt (last (:drawn (loser boards))))
            (apply +
                   (map #(Integer/parseInt %)
                        (filter
                         #(not (contains-elem? % (loserdrawn boards)))
                         (loservals boards))))))
  )))

