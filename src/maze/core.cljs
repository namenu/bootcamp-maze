(ns maze.core
  (:refer-clojure :exclude [print]))

(def dirs {:north [1 0]
           :south [-1 0]
           :east  [0 1]
           :west  [0 -1]})

(def opposite-dirs {:north :south
                    :south :north
                    :east  :west
                    :west  :east})

;; Grid is a hash-map of "pos" => "linked-walls"
;; Size metric is in it's metadata.

(defn full-grid [rows cols]
  (with-meta
    (into {} (for [i (range rows)
                   j (range cols)]
               {[i j] #{}}))
    {:size [rows cols]}))

(defn empty-grid [rows cols]
  (with-meta
    (into {} (for [r (range rows)
                   c (range cols)
                   :let [walls (cond-> #{}
                                       (> r 0) (conj :south)
                                       (> c 0) (conj :west)
                                       (< r (dec rows)) (conj :north)
                                       (< c (dec cols)) (conj :east))]]
               {[r c] walls}))
    {:size [rows cols]}))

(defn size [grid]
  (:size (meta grid)))

(defn all-pos [grid]
  (set (keys grid)))

(defn rand-pos [grid]
  (mapv rand-int (size grid)))

(defn linked-dirs [grid pos]
  (grid pos))

(defn advance [pos dir]
  (mapv + pos (dirs dir)))

(defn- break-wall [grid pos dir]
  (let [set-conj (fnil conj #{})]
    (update grid pos set-conj dir)))

(defn link-toward [grid pos dir]
  (-> grid
      (break-wall pos dir)
      (break-wall (advance pos dir) (opposite-dirs dir))))

(defn link-cells [grid pos neighbor]
  (let [v   (mapv - neighbor pos)
        dir (get {[1 0]  :north
                  [-1 0] :south
                  [0 1]  :east
                  [0 -1] :west}
                 v)]
    (-> grid
        (break-wall pos dir)
        (break-wall neighbor (opposite-dirs dir)))))

(defn unlink-toward [grid pos dir]
  (let [unbreak-wall (fn [grid pos dir]
                       (update grid pos disj dir))]
    (-> grid
        (unbreak-wall pos dir)
        (unbreak-wall (advance pos dir) (opposite-dirs dir)))))

(defn linked? [grid pos dir]
  (contains? (linked-dirs grid pos) dir))

(defn linked-cells [grid pos]
  (let [neighbor #(advance pos %)]
    (map neighbor (linked-dirs grid pos))))

(defn neighbor-dirs [grid [r c]]
  (let [[rows cols] (size grid)]
    (cond-> []
            (> r 0) (conj :south)
            (> c 0) (conj :west)
            (< r (dec rows)) (conj :north)
            (< c (dec cols)) (conj :east))))

(defn neighbor-cells [grid [r c]]
  (let [[rows cols] (size grid)]
    (cond-> []
            (> r 0) (conj [(dec r) c])
            (> c 0) (conj [r (dec c)])
            (< r (dec rows)) (conj [(inc r) c])
            (< c (dec cols)) (conj [r (inc c)]))))

(defn rand-neighbor [grid cur-pos candidates]
  (let [rand-nth0 (fn [coll] (if (empty? coll)
                               nil
                               (rand-nth coll)))]
    (->> (neighbor-cells grid cur-pos)
         (filter candidates)
         (rand-nth0))))


(defn print [grid]
  (with-out-str
    (let [[rows cols] (size grid)]

      (clojure.core/print "┼")
      (dotimes [_ cols]
        (clojure.core/print "───┼"))
      (println)

      (dotimes [i rows]
        (clojure.core/print "│")
        (doseq [s (->> (range cols)
                       (map #(linked? grid [i %] :east))
                       (map #(if % " " "│")))]
          (clojure.core/print (str "   " s)))
        (println)

        (clojure.core/print "┼")
        (doseq [s (->> (range cols)
                       (map #(linked? grid [i %] :north))
                       (map #(if % "   " "───")))]
          (clojure.core/print (str s "┼")))
        (println)))))
