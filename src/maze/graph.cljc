(ns maze.graph
  (:require [maze.core :refer [linked-cells]]))

(defn dijkstra [g src]
  (loop [frontier [src]
         dist     0
         visited  {src dist}]
    (if (empty? frontier)
      visited
      (let [frontier' (->> (mapcat #(linked-cells g %) frontier)
                           (remove #(contains? visited %)))
            visited'  (reduce #(assoc %1 %2 (inc dist)) visited frontier')]
        (recur frontier' (inc dist) visited')))))