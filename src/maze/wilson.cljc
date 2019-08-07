(ns maze.wilson
  (:require [maze.core :refer [full-grid all-pos rand-pos neighbor-dirs advance link-cells]]))

(defn- get-path [grid unvisited]
  (loop [cur-pos (rand-nth (seq unvisited))
         path    [cur-pos]]
    (let [next-dir  (rand-nth (neighbor-dirs grid cur-pos))
          next-pos  (advance cur-pos next-dir)
          idx-found (.indexOf path next-pos)]
      (if (contains? unvisited next-pos)
        (if (= idx-found -1)
          (recur next-pos (conj path next-pos))
          (recur next-pos (subvec path 0 (inc idx-found))))
        (conj path next-pos)))))

(defn- add-path [grid path]
  (reduce #(apply link-cells %1 %2) grid (partition 2 1 path)))

(deftype ^:private Wilson [output unvisited path]
  ISeqable
  (-seq [this] this)

  ISeq
  (-first [_] output)
  (-rest [_]
    (cond
      (seq path)
      (let [[from to] (first path)]
        (Wilson. (-> output
                     (update :grid link-cells from to)
                     (assoc :frontier [from]))
                 (disj unvisited from)
                 (rest path)))

      (seq unvisited)
      (let [path (get-path (:grid output) unvisited)]
        (Wilson. (-> output
                     (assoc :frontier [(first path)]))
                 unvisited
                 (partition 2 1 path))))))

(defn wilson
  "loop-erasing path finder"
  [rows cols]
  (let [grid      (full-grid rows cols)
        init-cell (rand-pos grid)
        unvisited (-> (all-pos grid)
                      (disj init-cell))]
    (Wilson. {:grid grid} unvisited nil)))
