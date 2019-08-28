(ns maze.seq.binary-tree
  (:require [maze.core :refer [full-grid size link-toward]])
  (:require-macros [maze.seq.macros :refer [defmaze]]))

(defn choose-wall [[rows cols] [r c]]
  (let [walls (cond-> []
                (< r (dec rows)) (conj :north)
                (< c (dec cols)) (conj :east))]
    (rand-nth walls)))

(defmaze BinaryTree [output nodes]
  (if-let [pos (first nodes)]
    (let [dir (choose-wall (:size output) pos)]
      (BinaryTree. (-> output
                       (update :grid link-toward pos dir)
                       (assoc :frontier [pos dir]))
                   (rest nodes)))))

(defn binary-tree [rows cols]
  (BinaryTree. {:size     [rows cols]
                :grid     (full-grid rows cols)
                :frontier nil}
               (->> (for [r (range rows) c (range cols)]
                      [r c])
                    (butlast))))

