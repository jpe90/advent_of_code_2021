(ns day17)

(defn update-position [{:keys [x y dx dy]}]
  {:x (+ x dx) :y (+ y dy) :dx (if (> dx 0) (- dx 1) 0) :dy (- dy 1)})

(defn past-bounds? [{:keys [x y]} {:keys [txe tys]}]
  (or (> tys y) (< txe x)))

(defn in-bounds? [{:keys [x y]} {:keys [txs txe tys tye]}]
  (and (<= tys y) (>= tye y) (<= txs x) (>= txe x)))

(defn seek-max
  ([dx dy bounds]
   (seek-max {:x 0 :y 0 :dx dx :dy dy} bounds))
  ([pos bounds]
   (when-not (past-bounds? pos bounds)
     (cons pos (lazy-seq (seek-max (update-position pos) bounds))))))

(defn valid-shot-for-bounds? [bounds] (fn [coll] (some #(in-bounds? % bounds) coll)))

(defn seek-for-bounds [bounds]
  (apply concat (for [dy (range -600 600)]
     (for [dx (range 1 600)]
       (seek-max dx dy bounds)))))

(defn valid-shots [bounds]
  (->> bounds seek-for-bounds (filter (valid-shot-for-bounds? bounds))))

(defn solve [bounds]
  (->> bounds valid-shots (map #(apply max (map :y %))) (apply max)))

(defn solve2 [bounds]
  (->> bounds valid-shots count))

(solve {:txs 155 :txe 182 :tys -117 :tye -67})              ;; part 1
(solve2 {:txs 155 :txe 182 :tys -117 :tye -67})             ;; part 2
