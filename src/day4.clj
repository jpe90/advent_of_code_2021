(ns day4
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.math.numeric-tower :as math :refer [expt]]
            [clojure.pprint :as pprint :refer [cl-format]]))

(def input (data/input 2021 4))
(def test-input )
(def test-input (slurp "src/tst4.txt"))

(defn extract-board [elem] (->> elem (map #(str/split % #"\s+")) flatten (filter not-empty)))

(defn parse-input [input] {:numbers (-> (->> input first) (str/split #","))
                           :boards (->> input
                                        rest
                                        (filter not-empty)
                                        (partition 5)
                                        (map extract-board))})
(defn run [in] (->> in
                    (str/split-lines)
                    parse-input
                    ;; (map )
                    ))
(map count (:boards (run input)))
(defn create-boards [list]
  (for [rows (range 5)]
    (take 5 list)))
;; just for convenience
(def boards (:boards (run input)))
(def numbers (:numbers (run input)))


(defn horizontals [board]
  (for [rows (range 5)]
    (let [sub-board (drop (* 5 rows) board)]
      (take 5 sub-board))))
(defn verticals [board]
  (for [rows (range 5)]
    (for [cols (range 5)]
      (nth board (+ (* cols 5) rows)))))

(defn diagonals [board]
  (let [asc (for [x (range 5)]
              (nth board (* x 6))
              )
        desc (for [x (range 5)]
               (nth board (+ 4 (* 4 x))))]
    (list asc desc)))
(defn mcontains? [item coll] (not= (.indexOf coll item) -1))
(defn notin? [item coll] (= (.indexOf coll item)  -1))
(defn not-contains-coll [little big] (every? #(notin? % little) big))
(defn contains-coll [little big] (every? #(mcontains? % little) big))
(defn check-win [board called]
  (let [to-check (->> (concat (horizontals board) (verticals board) (diagonals board)))
        winners (filter #(contains-coll called %) to-check)]
    (if (not-empty winners)
      board
      nil))
  )
(defn check-loss [board called]
  (let [to-check (->> (concat (horizontals board) (verticals board) (diagonals board)))
        winners (filter #(not-contains-coll called %) to-check)]
    (if (empty winners)
      board
      nil))
  )
(comment (defn check-loss [board called]
   (let [to-check (->> (concat (horizontals board) (verticals board) (diagonals board)))
         winners (filter #(not-contains-coll called %) to-check)]
     (if (not-empty winners)
       board
       nil))
   ))
(defn part2 [drawn]
  (let [winners (filter #(some? (check-loss % drawn)) boards)]
    (if (= (count winners) 1)
      {:drawn drawn :winners winners}
      nil)))
(def part1 (loop [x 1]
   (let [drawn (take x numbers)
         winners (filter #(some? (check-win % drawn)) boards)]
     (if (= (count winners) (- (count boards) 1))
       {:drawn drawn :winners (count winners)}
       (recur (inc x))))))

(def part1 (loop [x 1]
   (let [drawn (take x numbers)
         winners (filter #(some? (check-win % drawn)) boards)
         losers (filter #(not (some? (check-win % drawn))) boards) ]
     (if (= (count winners) (- (count boards) 1))
       {:drawn drawn :winners losers}
       (recur (inc x))))))

(defn check-winx [board called]
  (let [to-check (->> (concat (horizontals board) (verticals board)))
        winners (filter #(contains-coll called %) to-check)]
    (if (not-empty winners)
      board
      nil))
  )

(defn part1x [board] (loop [x 1]
                 (let [drawn2 (take x numbers)
                       winners (filter #(some? (check-winx % drawn2)) board)]
                   (if (not-empty winners)
                     {:drawn drawn2 :winners winners :lendrawn (count drawn2)}
                     (recur (inc x))))))
(def part3 (part1x (:winners part1)))
(map #(check-loss % (:drawn part1)) boards)

(def loser (last (sort-by :lendrawn (map #(part1x (list %)) boards))))
(def loservals (first (:winners loser)))
(def loserdrawn (:drawn loser))
(* (Integer/parseInt (last (:drawn loser))) (apply + (map #(Integer/parseInt %) (filter #(notin? % loserdrawn) loservals))))



(notin? 4 '(1 2 3 4))


(check-win (first boards) '("40" "75" "77" "38" "22" "9000"))

(def called (->> (:drawn part3) (map #(Integer/parseInt %))))
(def justcalled (last called))

(def sum (->> (first (:winners part3)) (map #(Integer/parseInt %)) (filter #(notin? % called)) (apply +)))
(def sum2 (->> (first (:winners part3)) (map #(Integer/parseInt %)) (filter #(notin? % called))))

(* sum justcalled)
