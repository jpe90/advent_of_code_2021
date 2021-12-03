(ns day3
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.math.numeric-tower :as math :refer [expt]]
            [clojure.pprint :as pprint :refer [cl-format]]))

;; do a list of parsed nums
;; make freq list by summing bitw
(def input (data/input 2021 3))
(def test-input (slurp "src/inp.txt"))

(defn bin-string->num-list [bin-string] (map #(. Character getNumericValue %) bin-string))
(defn- mk-map [in] {:input-size (count in) :frequency-list (map bin-string->num-list in)})
(defn- mk-map2 [in] {:input-size (count in) :frequency-list in})

(defn freq-info [in] (->> in (str/split-lines) mk-map))
(defn freq-info2 [in] (->> in (str/split-lines) mk-map2))



(defn frequency-list [in]
  (update (freq-info in) :frequency-list (fn [lst] (reduce #(map + %1 %2) lst))))

(defn gamma-rate-list [freq-list]
  (let [most-occuring (fn [num] (if (< (/ (:input-size freq-list) 2) num) 0 1))]
    (map most-occuring (:frequency-list freq-list))))


(defn gamma-decimal [in] (->> in (reduce str) (str "2r") read-string))
(defn epsilon-decimal [in] (->> in bit-not (bit-and 2r111111111111)))
(defn epsilon-decimal2 [in] (->> in bit-not (bit-and 2r11111)))
(defn int->bin-list [int] (map #(. Character getNumericValue %) (cl-format nil "~12,'0b" int)))
(defn int->bin-list2 [int] (map #(. Character getNumericValue %) (cl-format nil "~5,'0b" int)))
;; (defn rating [filter-num binary-numbers]
;;   (let [bit-comp (fn [cmp num])]
;;     (loop [remaining-numbers (:frequency-list binary-numbers)
;;            idx 12]
;;       (if (> 1 (count remaining-numbers))))))
(let [test-input-frequency (frequency-list input)
      gamma-list (gamma-rate-list test-input-frequency)
      gamma (gamma-decimal gamma-list)
      epsilon (epsilon-decimal gamma)]
  (* gamma epsilon)
  )


(* 349 3746)
(Integer/toString 5 2)
(cl-format nil "~8,'0b" 23)
;; (defn int->bin-list [int] (map #(. Character getNumericValue %) (Integer/toString int 2)))

(int->bin-list 349)


;; it's truncating the number- we need to bitwise and 
(defn filter-bin-list [list]
  #break (loop [ind 0
         remaining-list list]
   #break (let [num (:frequency-list (mk-map2 list))
          digit (nth num ind)
          filtered-list (filter #(= (nth % ind) digit) remaining-list)]
      (if (> (count filtered-list) 1)
        (recur (inc ind) filtered-list)
        ;; #break filtered-list
        (first filtered-list)))))

(let [test-input-frequency (frequency-list input)
      gamma-list (gamma-rate-list test-input-frequency)
      gamma (gamma-decimal gamma-list)
      epsilon (epsilon-decimal gamma)]
  (int->bin-list gamma)
  )


(defn bin-list->int [lst]  (Integer/parseInt (reduce str lst) 2))
(defn solve [fst snd] (* (bin-list->int fst) (bin-list->int snd)))

(defn testgen [val]
  (let [test-input-frequency (frequency-list test-input)
        gamma-list (gamma-rate-list test-input-frequency)
        gamma (gamma-decimal gamma-list)
        epsilon (epsilon-decimal2 gamma)]
    (if (= 1 val)
      (filter-bin-list (:frequency-list (freq-info test-input)))
      (filter-bin-list (:frequency-list (freq-info test-input))))
    ))

(defn wtf [val]
  #break (let [test-input-frequency (frequency-list input)
        gamma-list (gamma-rate-list test-input-frequency)
        gamma (gamma-decimal gamma-list)
        epsilon (epsilon-decimal gamma)]
    (if (= 1 val)
      (filter-bin-list (freq-info2 input))
      (filter-bin-list (freq-info2 input)))
    ))


(defn lister [list]
  (let [sum (reduce #(map + %1 %2) list)
        cnt (count list)
        avgs (map #(/ % cnt) sum)
        greaterone (map #(if (<= (/ 1 2) %) 1 0) avgs)]
    greaterone)
  )
(defn notlister [list]
  (let [sum (reduce #(map + %1 %2) list)
        cnt (count list)
        avgs (map #(/ % cnt) sum)
        greaterone (map #(if (> (/ 1 2) %) 1 0) avgs)]
    greaterone)
  )
(defn freq-info3 [in] (->> in (str/split-lines) (map bin-string->num-list)))
(def info3 (freq-info3 input))

(defn filter-bin-list [list]
  (loop [ind 0
         remaining-list list]
    (let [num (lister remaining-list)
          digit (nth num ind)
          filtered-list (filter #(= (nth % ind) digit) remaining-list)]
      (if (> (count filtered-list) 1)
        (recur (inc ind) filtered-list)
        ;; #break filtered-list
        (first filtered-list)))))

(defn filter-bin-list2 [list]
  (loop [ind 0
         remaining-list list]
    (let [num (notlister remaining-list)
          digit (nth num ind)
          filtered-list (filter #(= (nth % ind) digit) remaining-list)]
      (if (> (count filtered-list) 1)
        (recur (inc ind) filtered-list)
        ;; #break filtered-list
        (first filtered-list)))))

(defn solve [lst]
  (let [i (filter-bin-list lst)
        j (filter-bin-list2 lst)
        a (bin-list->int i)
        b (bin-list->int j)]
    #break (* a b)))

(solve (freq-info3 input))




;; (let [test-input-frequency (frequency-list test-input)
;;         gamma-list (gamma-rate-list test-input-frequency)
;;         gamma (gamma-decimal gamma-list)
;;         epsilon (epsilon-decimal2 gamma)]
;;     gamma-list
;;     )
;; (solve (testgen 1) (testgen 0))
;; (solve (wtf 1) (wtf 0))
;; (solve ep gm)




                                        ;(let [lst [4 8 9]
                                        ;      f [[2 3 7]
                                        ;         [4 8 2]
                                        ;         [4 1 1]]]
                                        ;  (for [x (range 0 3)]
                                        ;    ))
                                        ;
                                        ;
                                        ;

