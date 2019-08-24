(ns maze.seq.recursive-division
  (:refer-clojure :exclude [divide])
  (:require [maze.core :refer [empty-grid unlink-toward print]]))

(declare divide-vertically)
(declare divide-horizontally)

(defn divide [[row col height width :as area]]
  (when (and (> width 1) (> height 1))
    (if (>= width height)
      (divide-vertically area)
      (divide-horizontally area))))

(defn divide-vertically [[row col height width :as area]]
  (let [offset (rand-int (dec width))
        col'   (+ col offset)
        left   [row col height (inc offset)]
        right  [row (inc col') height (- width offset 1)]]
    (lazy-seq
      (cons [area :vertical col'] (mapcat divide [left right])))))

(defn divide-horizontally [[row col height width :as area]]
  (let [offset (rand-int (dec height))
        row'   (+ row offset)
        top    [row col (inc offset) width]
        bottom [(inc row') col (- height offset 1) width]]
    (lazy-seq
      (cons [area :horizontal row'] (mapcat divide [top bottom])))))

(defn make-walls [[row col height width] orientation axis]
  (if (= orientation :vertical)
    (let [passage-r (+ row (rand-int height))]
      (for [r (range height)
            :let [row' (+ row r)]
            :when (not= row' passage-r)]
        [[row' axis] :east]))
    (let [passage-c (+ col (rand-int width))]
      (for [c (range width)
            :let [col' (+ col c)]
            :when (not= col' passage-c)]
        [[axis col'] :north]))))

(deftype ^:private RecursiveDivision [output walls]
  ISeqable
  (-seq [this] this)

  ISeq
  (-first [_] output)
  (-rest [_]
    (when-let [wall (first walls)]
      (RecursiveDivision. (-> output
                              (update :grid #(apply unlink-toward % wall))
                              (assoc :frontier wall))
                          (rest walls)))))

(defn recursive-division [rows cols]
  (let [div-seq (divide [0 0 rows cols])
        walls   (mapcat #(apply make-walls %) div-seq)]
    (RecursiveDivision. {:grid (empty-grid rows cols)}
                        walls)))