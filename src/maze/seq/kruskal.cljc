(ns maze.seq.kruskal
  (:require [maze.core :refer [full-grid advance link-toward]]
            [clojure.set :refer [union]]))

(defn find-tree [forest node]
  (first (filter #(% node) forest)))

(defn span-tree [forest [u dir]]
  (let [v  (advance u dir)
        t1 (find-tree forest u)
        t2 (find-tree forest v)]
    (if (not= t1 t2)
      (-> forest
          (disj t1)
          (disj t2)
          (conj (union t1 t2))))))

(deftype ^:private Kruskal [output forest edges]
  ISeqable
  (-seq [this] this)

  ISeq
  (-first [_] output)
  (-rest [_]
    (if (> (count forest) 1)
      (loop [forest forest
             [e & edges'] edges]
        (if-let [forest' (span-tree forest e)]
          (let [[u dir] e]
            (Kruskal. (-> output
                          (update :grid link-toward u dir)
                          (assoc :frontier [u]))
                      forest'
                      edges'))
          (recur forest edges'))))))

(defn kruskal [rows cols]
  (let [grid     (full-grid rows cols)
        vertices (for [r (range rows)
                       c (range cols)]
                   [r c])
        walls    (fn [[r c :as pos]]
                   (cond-> []
                     (< r (dec rows)) (conj [pos :north])
                     (< c (dec cols)) (conj [pos :east])))
        edges    (mapcat walls vertices)]
    (Kruskal. {:grid grid}
              (into #{} (map (comp set list) vertices))
              (shuffle edges))))
