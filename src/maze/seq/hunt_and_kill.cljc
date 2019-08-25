(ns maze.seq.hunt-and-kill
  (:require [maze.core :refer [full-grid rand-pos all-pos neighbor-cells rand-neighbor link-cells]]))

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn hunt
  "Find a new starting point which is unvisited yet adjacent to visited cell(s)."
  [grid visited unvisited]
  (find-first #(some visited (neighbor-cells grid %)) unvisited))

(deftype ^:private HuntAndKill [output cur-pos visited unvisited]
  ISeqable
  (-seq [this] this)

  ISeq
  (-first [_] output)
  (-rest [_] (if-not (empty? unvisited)
               (let [grid (:grid output)]
                 (if-let [new-pos (rand-neighbor grid cur-pos unvisited)]
                   (HuntAndKill. (-> output
                                     (update :grid link-cells cur-pos new-pos)
                                     (assoc :frontier [new-pos]))
                                 new-pos
                                 (conj visited new-pos)
                                 (disj unvisited new-pos))
                   (let [new-cur-pos (hunt grid visited unvisited)
                         adj-cell    (rand-neighbor grid new-cur-pos visited)]
                     (HuntAndKill. (-> output
                                       (update :grid link-cells new-cur-pos adj-cell)
                                       (assoc :frontier [new-cur-pos]))
                                   new-cur-pos
                                   (conj visited new-cur-pos)
                                   (disj unvisited new-cur-pos))))))))

(defn hunt-and-kill [rows cols]
  (let [grid      (full-grid rows cols)
        init-pos  (rand-pos grid)
        unvisited (-> (all-pos grid)
                      (disj init-pos))]
    (HuntAndKill. {:grid     grid
                   :frontier [init-pos]}
                  init-pos
                  #{init-pos}
                  unvisited)))
