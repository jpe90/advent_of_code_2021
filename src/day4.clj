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

(defn check-win [board called]
  (let [to-check (->> (concat (horizontals board) (verticals board)))
        winners (filter #(contains-coll called %) to-check)]
    (if (not-empty winners)
      board
      nil))
  )

(defn win-state [board numbers] (loop [x 1]
                                  (let [drawn-numbers (take x numbers)
                                        win-state-reached? (some? (check-win board drawn-numbers))]
                                    (if win-state-reached?
                                      {:drawn drawn-numbers :winning-board board}
                                      (recur (inc x))))))


(defn ordered-win-states [boards numbers]
  (->> boards
       (map #(win-state % numbers))
       (sort-by #(count (:drawn %)))))

(defn select-win-state [f]
  (let [run (run input)
        boards (:game-boards run)
        numbers (:drawn-numbers run)
        last-win-state (f (ordered-win-states boards numbers))
        last-winning-board (map #(Integer/parseInt %)(:winning-board last-win-state))
        called-numbers (map #(Integer/parseInt %) (:drawn last-win-state))
        last-number-called (last called-numbers)
        uncalled-numbers (filter #(not (contains-elem? % called-numbers)) last-winning-board)
        sum-uncalled (apply + uncalled-numbers)]
    (* sum-uncalled last-number-called)))

;; part 1
(select-win-state first)
;; part 2
(select-win-state last)






