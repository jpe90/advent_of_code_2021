(ns day16
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(def data (data/input 2021 16))

(defn hex-char->bin-seq [chr] (-> chr str (Integer/parseInt 16) (Integer/toString 2) (Integer/parseInt) (->> (format "%04d"))))
(defn bin-seq->long [bin] (Long/parseLong (apply str bin) 2))
(defn hex-str->bin [in] (mapcat hex-char->bin-seq in))
(def ext (hex-str->bin (str/trim data)))

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
  (merge m (parse-literal payload)))

(declare process-packet)

(defn process-variable-operator [region]
  (loop [remaining region
         children []]
    (let [current (process-packet remaining)
          rem (:remaining current)
          result (cons current children)]
      (if (not-empty rem)
        (recur rem result)
        {:value (reverse result) :remaining rem}))))

(defn process-fixednum-operator [region num]
  (loop [remaining region
         iter num
         children []]
    (let [current (process-packet remaining)
          rem (:remaining current)
          result (cons current children)]
      (if (= iter 1)
        {:value (reverse result) :remaining rem}
        (recur rem (dec iter) result)))))

(defn process-packet [bits]
  (let [version (take 3 bits)
        id ((comp #(take 3 %) #(drop 3 %)) bits)
        header {:version version :id id}
        payload (drop 6 bits)
        process-operator (fn [{:keys [version id]} pyld]
                           (let [lid (first pyld)
                                 length-subp-bits (bin-seq->long (take 15 (rest pyld)))
                                 num-subp-bin (bin-seq->long (take 11 (rest pyld)))
                                 variable-region (take length-subp-bits (drop 16 pyld))
                                 variable-rest (drop (+ 16 length-subp-bits) pyld)
                                 fixed-region (drop 12 pyld)]
                             (if (= \0 lid)
                               {:value (process-variable-operator variable-region) :remaining variable-rest}
                               (process-fixednum-operator fixed-region num-subp-bin))))]
    (if (= id [\1 \0 \0])
      (process-literal header payload)
      (let [result (process-operator header payload)]
        (merge header result)))))

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
      (map? packets) (if (= [\1 \0 \0] id) val (if (some? id) (apply (id->op id) (perform-ops val)) (perform-ops val)))
      (list? packets) (map perform-ops packets)
      :else val)))

(perform-ops (process-packet ext))
