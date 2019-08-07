(ns maze.generator
  (:require [maze.binary-tree :refer [binary-tree]]
            [maze.sidewinder :refer [sidewinder]]
            [maze.aldous-broder :refer [aldous-broder]]
            [maze.wilson :refer [wilson]]
            [maze.hunt-and-kill :refer [hunt-and-kill]]
            [maze.recursive-backtracker :refer [recursive-backtracker]]
            [maze.kruskal :refer [kruskal]]))

(def algorithms {:binary-tree           ["Binary Tree" binary-tree]
                 :sidewinder            ["Sidewinder" sidewinder]
                 :aldous-broder         ["Aldous Broder" aldous-broder]
                 :wilson                ["Wilson" wilson]
                 :hunt-and-kill         ["Hunt-and-Kill" hunt-and-kill]
                 :recursive-backtracker ["Recursive Backtracker" recursive-backtracker]
                 :kruskal               ["Kruskal" kruskal]})
