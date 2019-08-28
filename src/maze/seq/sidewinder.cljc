(ns maze.seq.sidewinder
  (:require [maze.core :refer [full-grid size link-toward advance]])
  (:require-macros [maze.seq.macros :refer [defmaze]]))

(defn close-out?
  "옆으로 진행하기를 멈추고 아래를 확장할 것인지?"
  [[rows cols] [r c]]
  (or (= c (dec cols))
      (and (< r (dec rows))
           (rand-nth [true false]))))

(defmaze Sidewinder [output coords run]
  (if-let [pos (first coords)]
    (let [run (conj run pos)
          [pos dir new-run] (if (close-out? (:size output) pos)
                              [(rand-nth run) :north []]
                              [pos :east run])]
      (Sidewinder. (-> output
                       (update :grid link-toward pos dir)
                       (assoc :frontier [(advance pos dir)]))
                   (rest coords)
                   new-run))))

(defn sidewinder [rows cols]
  (Sidewinder. {:size     [rows cols]
                :grid     (full-grid rows cols)
                :frontier nil}
               (->> (for [i (range rows) j (range cols)]
                      [i j])
                    (butlast))
               []))
