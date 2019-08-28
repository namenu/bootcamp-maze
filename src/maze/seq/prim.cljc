(ns maze.seq.prim
  (:require [maze.core :refer [print size full-grid rand-pos neighbor-cells link-cells]])
  (:require-macros [maze.seq.macros :refer [defmaze]]))

(def map-vec (partial map vector))

(defn add-edge
  "NOT SAFE: when randomly selected weight is duplicated, previous edge will be dropped."
  [edges e]
  (assoc edges (rand) e))

(defn select-unvisited [edges visited]
  (loop [edges edges]
    (if-let [[w [src dst]] (first edges)]
      (if (visited dst)
        (recur (dissoc edges w))
        [[src dst] (dissoc edges w)]))))

(defmaze SimplifiedPrim [output edges visited]
  (let [grid (:grid output)]
    (if (< visited (size grid))
      (if-let [[[src dst] edges'] (select-unvisited edges visited)]
        (let [edges'   (->> (neighbor-cells grid dst)
                            (remove #(contains? visited %))
                            (map vector (repeat dst))
                            (reduce add-edge edges'))
              visited' (conj visited dst)]
          (SimplifiedPrim. (-> output
                               (update :grid link-cells src dst)
                               (assoc :frontier [dst])) edges' visited'))))))

(defn simplified-prim [rows cols]
  (let [grid (full-grid rows cols)
        root (rand-pos grid)]
    (SimplifiedPrim. {:grid grid}
                     (->> (map vector (repeat root) (neighbor-cells grid root))
                          (reduce add-edge (sorted-map)))
                     #{root})))
