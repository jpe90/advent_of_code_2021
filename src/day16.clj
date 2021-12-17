(ns day16
  (:require [aocd.core :as data]
            [clojure.string :as str]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.pprint :refer [cl-format]]
            [clojure.walk :as w]))

(def data (data/input 2021 16))
(def ex1 (hex-str->bin "8A004A801A8002F478"))
(def ex2 (hex-str->bin "620080001611562C8802118E34"))
(def ex3 (hex-str->bin "C0015000016115A2E0802F182340"))
(def ex4 (hex-str->bin "A0016C880162017C3686B18A3D4780"))
(def ex9 (hex-str->bin "CE00C43D881120"))
(def ext (hex-str->bin (str/trim data)))

(defn hex-char->bin-seq [chr] (-> chr str (Integer/parseInt 16) (Integer/toString 2) (Integer/parseInt) (->> (format "%04d"))))
(defn bin-seq->long [bin] (Long/parseLong (apply str bin) 2))
(defn hex-str->bin [in] (mapcat hex-char->bin-seq in))

(defn id->op [bin]
  (case bin
    [\0 \0 \0] +
    [\0 \0 \1] *
    [\0 \1 \0] min
    [\0 \1 \1] max
    [\1 \0 \1] #(if (> %1 %2) 1 0)
    [\1 \1 \0] #(if (< %1 %2) 1 0)
    [\1 \1 \1] #(if (= %1 %2) 1 0)))

(defn parse-literal
  ([bits] (parse-literal bits []))
  ([current acc]
   (let [nib (take 4 (rest current))
         remaining (drop 5 current)]
     (if (= \1 (first current))
       (recur (drop 5 current) (concat acc nib))
       {:value (bin-seq->long (concat acc nib)) :remaining remaining}))))

(defn process-literal [m payload]
  (do
    (println "in process literal with bits " (take 10 payload) " ... count " (count payload))
    (merge m (parse-literal payload))))

(declare process-packet)

(defn process-variable-operator [region]
  (loop [remaining region
         children []]
    (let [current (process-packet remaining)
          rem (:remaining current)
          result (cons current children)]
      (if (not-empty rem)
        (do
          (println "Variable op for region " region " count " (count region))
          (recur rem result))
                                        ;(reverse result)
        (do (println "variable op returning with remaining" rem) {:value (reverse result) :remaining rem})))))

(defn process-fixednum-operator [region num]
  (loop [remaining region
         iter num
         children []]
    (let [current (process-packet remaining)
          ;; and then it's available here [2]
          rem (:remaining current)
          result (cons current children)]
      (do
        (println "Fixednum op, iteration " iter " for region " region " count " (count region))
        (if (= iter 1)
          (do (println "fixednum returning with remaining " rem) {:value (reverse result) :remaining rem})
          (do
            (println "recurring with (should always have a value) remaining: " rem)
            ;; this failed because process packet ate everything where we were expecting to only it what it needed and return the rest as :remaining
            (recur rem (dec iter) result)))))))

(defn process-packet [bits]
  (do
    (println "Processing packet: " (take 20 bits) " ... count " (count bits))
    (let [version (take 3 bits)
          id ((comp #(take 3 %) #(drop 3 %)) bits)
          header {:version version :id id}
          payload (drop 6 bits)
          process-operator (fn [{:keys [version id]} pyld]
                             (let [lid (first pyld)
                                   length-subp-bits (bin-seq->long (take 15 (rest pyld)))
                                   num-subp-bin (bin-seq->long (take 11 (rest pyld)))
                                   ;; we send it variable op this slice but we don't process the rest
                                   variable-region (take length-subp-bits (drop 16 pyld))
                                   ;; we need to tack on variable-rest and make sure fixednum rest comes too
                                   variable-rest (drop (+ 16 length-subp-bits) pyld)
                                   fixed-region (drop 12 pyld)]
                               (do
                                 (println "Processing Operator, header " header " ... count " (count pyld))
                                 (if (= \0 lid)
                                   {:value (process-variable-operator variable-region) :remaining variable-rest}
                                   ;; and here we don't return the remaining either maybe. or maybe we do
                                   (process-fixednum-operator fixed-region num-subp-bin)))))]
      (if (= id [\1 \0 \0])
        ;; bubble up remaining here [1]
        (process-literal header payload)
        (let [result (process-operator header payload)]
          (merge header result))))))

;; part 1

(defn sum-versions [packets]
  (let [val (:value packets)
        vers (:version packets)]
    (cond
      (map? val)  (cons (bin-seq->long vers) (sum-versions val))
      (list? val) (if (some? (:id packets)) (cons (bin-seq->long vers) (map sum-versions val)) (map sum-versions val))
      :else (bin-seq->long vers))))

;; part 2

(defn perform-ops [packets]
  (let [val (:value packets)
        id (:id packets)]
    (cond
      (map? packets) (if (= [\1 \0 \0] id) val (if (some? id) {:op id :children (perform-ops val)} (perform-ops val)))
      (list? packets) (map perform-ops packets)
      :else val)))

(defn perform-ops [packets]
  (let [val (:value packets)
        id (:id packets)]
    (cond
      (map? packets) (if (= [\1 \0 \0] id) val (if (some? id) (apply (id->op id) (perform-ops val)) (perform-ops val)))
      (list? packets) (map perform-ops packets)
      :else val)))

(perform-ops (process-packet ext))
