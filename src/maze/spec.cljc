(ns maze.spec
  (:require [maze.core :as m]
            [cljs.spec.alpha :as s]))


(s/def ::dir #{:north :south :east :west})
(s/def ::pos (s/tuple int? int?))
(s/def ::grid map?)

(s/fdef m/advance
  :args (s/cat :pos ::pos
               :dir ::dir)
  :ret ::pos)

(s/fdef m/link-cells
  :args (s/cat :grid string?
               :pos ::pos
               :neighbor ::pos)
  :ret ::grid)

(s/fdef m/linked?
  :args (s/cat :grid ::grid
               :pos ::pos
               :dir ::dir)
  :ret boolean?)

(s/fdef m/linked-cells
  :args (s/cat :grid ::grid
               :pos ::pos)
  :ret (s/coll-of ::pos))
