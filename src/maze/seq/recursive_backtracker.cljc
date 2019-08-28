(ns maze.seq.recursive-backtracker
  (:require [maze.core :refer [full-grid rand-pos all-pos rand-neighbor link-cells]])
  (:require-macros [maze.seq.macros :refer [defmaze]]))

(defmaze RecursiveBacktracker [output stack unvisited]
  (if-not (empty? unvisited)
    (let [cur-pos (peek stack)]
      (if-let [new-pos (rand-neighbor (:grid output) cur-pos unvisited)]
        (RecursiveBacktracker. (-> output
                                   (update :grid link-cells cur-pos new-pos)
                                   (assoc :frontier [new-pos]))
                               (conj stack new-pos)
                               (disj unvisited new-pos))
        (RecursiveBacktracker. (assoc output :frontier [cur-pos])
                               (pop stack)
                               unvisited)))))

(defn recursive-backtracker [rows cols]
  (let [grid (full-grid rows cols)
        init-pos (rand-pos grid)
        unvisited (disj (all-pos grid) init-pos)]
    (RecursiveBacktracker. {:grid grid
                            :frontier [init-pos]}
                           [init-pos]
                           unvisited)))
