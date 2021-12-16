(ns day13
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]))

(def test-input (slurp "src/13.txt"))

(defn parse-point [line] (let [vals (map #(Long/parseLong %) (str/split line #","))]
                           {:x (first vals) :y (second vals)}))
(defn parse-instruction [line]
  (let [res (str/split line #"=")
        direction (keyword (last (str/split (first res) #"\s+")))
        value (Long/parseLong (second res))]
    {direction value}))
(defn delegate-line [line]
  (cond
    (str/includes? line ",") (parse-point line)
    (str/includes? line "=") (parse-instruction line)
    :else nil))
(defn points->points-info [pts] {:points pts :ymax (apply max (map :y pts)) :xmax (apply max (map :x pts))})
(defn parse-input [input]
  (as-> input i
    (str/split-lines i)
    (mapv delegate-line i)
    (split-with some? i)
    (merge (points->points-info(first i)) {:instructions (filter some? (second i))})
    ))

;; if line has a comma, add to points
;; if it has an equal, add to instructions
(def test (parse-input test-input))
(def real (parse-input (data/input 2021 13)))

                                        ;
;; not intuitive what's going on here 
(defn update-x-pts [oldmax newmax]
  (fn [current] (distinct (map #(hash-map :x (if (> (:x %) newmax)
                                               (- oldmax (:x %))
                                               (:x %))
                                          :y (:y %)) current))))

(defn update-y-pts [oldmax newmax]
  (fn [current] (distinct (map #(hash-map :x (:x %)
                                          :y (if (> (:y %) newmax)
                                               (- oldmax (:y %))
                                               (:y %))) current))))

(defn fold-y [pts-info fy]
  (as-> pts-info i
    (assoc i :ymax fy)
    (update i :points (update-y-pts (:ymax pts-info) fy))
    (update i :instructions rest)
    ))

(defn fold-x [pts-info fx]
  (as-> pts-info i
    (assoc i :xmax fx)
    (update i :points (update-x-pts (:xmax pts-info) fx))
    (update i :instructions rest)
    ))


                                        ;(map #(hash-map :x (:x %) :y (if (> 2 (:y %)) 42 (:y %))) '({:x 6, :y 10}
                                        ;            {:x 0, :y 14}
                                        ;            {:x 9, :y 10}
                                        ;            {:x 0, :y 3}
                                        ;            {:x 10, :y 4}
                                        ;            {:x 4, :y 11}
                                        ;            {:x 6, :y 0}
                                        ;            {:x 6, :y 12}
                                        ;            {:x 4, :y 1}
                                        ;            {:x 0, :y 13}
                                        ;            {:x 10, :y 12}
                                        ;            {:x 3, :y 4}
                                        ;            {:x 3, :y 0}
                                        ;            {:x 8, :y 4}
                                        ;            {:x 1, :y 10}
                                        ;            {:x 2, :y 14}
                                        ;            {:x 8, :y 10}
                                        ;            {:x 9, :y 0}))
(count (:points (fold-y test 7)))
;; (count (:points (fold-x real 655)))

(defn looper [in]
  (loop [x in]
    (let [ins (first (:instructions x))]
      (cond
        (some? (:x ins)) (recur (fold-x x (:x ins)))
        (some? (:y ins)) (recur (fold-y x (:y ins)))
        :else x))))

(defn looper [in]
  (loop [x in]
    (let [ins (first (:instructions x))]
      (cond
        (some? (:x ins)) (recur (fold-x x (:x ins)))
        (some? (:y ins)) (recur (fold-y x (:y ins)))
        :else x))))

(defn mcontains? [coll val] (some #(= val %) coll))
(defn prn [in]
  (for [y (range (:ymax in))]
    (for [x (range (:xmax in))]
      (let [prnval (if (mcontains?  (:points in) {:x x :y y}) "#" ".")]
        (if (= x (- (:xmax in) 1))
          (print (str prnval "\n"))
          (print prnval))))
    ))
(mcontains?  (:points test) {:x 6 :y 10})
(prn (looper real))

(prn (looper test))



                                        ;(defn fold [points ins]
                                        ;  (if (some? (:x ins)) :x :y)
                                        ;
                                        ;  )
                                        ;
                                        ;(fold "e" {:x "b"})




;; => ["6,10" "0,14" "9,10" "0,3" "10,4" "4,11" "6,0" "6,12" "4,1" "0,13" "10,12" "3,4" "3,0" "8,4" "1,10" "2,14" "8,10" "9,0" "" "fold along y=7" "fold along x=5"]


;; defn for fold y - takes a list of points and a y pivot
;; map all y coords of points above pivot to (val - pivot)
;; distinct on that

;; same for fold x- 

;; (let [parsed-input (-> (data/input 2021 7) parse-input)]
;;   (cl-format true "Part 1: ~d~%" )
;;   (cl-format true "Part 2: ~d~%" ))

