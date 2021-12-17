(ns day17)

(def step-x (memoize (fn [dx] (if (> dx 0) (- dx 1) 0))))
(def step-y (memoize (fn [dy] (- dy 1))))

(defn update-position [{:keys [x y dx dy]}]
  {:x (+ x dx)
   :y (+ y dy)
   :dx (step-x dx)
   :dy (step-y dy)})

(defn past-bounds? [{:keys [x y]} {:keys [txe tys]}]
  (or (> tys y) (< txe x)))

(defn in-bounds? [{:keys [x y]} {:keys [txs txe tys tye]}]
  (and (<= tys y) (>= tye y) (<= txs x) (>= txe x)))

(defn seek-max
  ([dx dy bounds]
   (seek-max {:x 0 :y 0 :dx dx :dy dy} bounds))
  ([pos bounds]
   (if (past-bounds? pos bounds)
     nil
     (cons pos (lazy-seq (seek-max (update-position pos) bounds))))))

(defn valid-shot-for-bounds? [bounds] (fn [coll] (some #(in-bounds? % bounds) coll)))

(defn seek-for-bounds [bounds]
  (apply concat (for [dy (range -600 600)]
     (for [dx (range 1 600)]
       (seek-max dx dy bounds)))))

(defn solve [bounds]
  (->> bounds
       seek-for-bounds
       (filter (valid-shot-for-bounds? bounds))
       (map #(map :y %))
       (map #(apply max %))
       (apply max)))

(defn solve2 [bounds]
  (->> bounds
       seek-for-bounds
       (filter (valid-shot-for-bounds? bounds))
       count))

(solve {:txs 155 :txe 182 :tys -117 :tye -67})              ;; part 1
(solve2 {:txs 155 :txe 182 :tys -117 :tye -67})             ;; part 2