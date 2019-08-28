(ns maze.seq.aldous-broder
  (:require [maze.core :refer [full-grid rand-pos neighbor-dirs advance linked-cells link-toward]]))

(deftype ^:private AldousBroder [output cur-pos unvisited counter]
  ISeqable
  (-seq [this] this)

  ISeq
  (-first [_] output)
  (-rest [_] (if (and (pos? unvisited)
                      (pos? counter))
               (let [grid     (:grid output)
                     next-dir (rand-nth (neighbor-dirs grid cur-pos))
                     next-pos (advance cur-pos next-dir)]
                 (if (empty? (linked-cells grid next-pos))  ; single cell?
                   (AldousBroder. (-> output
                                      (update :grid link-toward cur-pos next-dir)
                                      (assoc :frontier [next-pos]))
                                  next-pos
                                  (dec unvisited)
                                  (dec counter))
                   (AldousBroder. (assoc output :frontier [next-pos])
                                  next-pos
                                  unvisited
                                  (dec counter)))))))

(defn aldous-broder
  "Randomly walks until all cells are connected.
  Or, it stops when number of steps is reached to 'size^2' to prevent long-running."
  [rows cols]
  (let [grid (full-grid rows cols)]
    (AldousBroder. {:grid grid}
                   (rand-pos grid)
                   (dec (* rows cols))
                   (* (* rows cols) (* rows cols)))))
