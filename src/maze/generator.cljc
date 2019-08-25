(ns maze.generator
  (:require [maze.seq.binary-tree :refer [binary-tree]]
            [maze.seq.sidewinder :refer [sidewinder]]
            [maze.seq.aldous-broder :refer [aldous-broder]]
            [maze.seq.wilson :refer [wilson]]
            [maze.seq.hunt-and-kill :refer [hunt-and-kill]]
            [maze.seq.recursive-backtracker :refer [recursive-backtracker]]
            [maze.seq.kruskal :refer [kruskal]]
            [maze.seq.prim :refer [simplified-prim]]
            [maze.seq.recursive-division :refer [recursive-division]]))

(def biased
  {:binary-tree ["Binary Tree" binary-tree]
   :sidewinder  ["Sidewinder" sidewinder]})

(def random-walk
  {:aldous-broder ["Aldous Broder" aldous-broder]
   :wilson        ["Wilson" wilson]
   :hunt-and-kill ["Hunt-and-Kill" hunt-and-kill]})

(def growing-tree
  {:recursive-backtracker ["Recursive Backtracker" recursive-backtracker]
   :kruskal               ["Kruskal" kruskal]
   :prim                  ["Prim" simplified-prim]})

(def not-categorized
  {:recursive-division ["Recursive Division" recursive-division]})

(def algorithms
  (merge biased
         random-walk
         growing-tree
         not-categorized))
