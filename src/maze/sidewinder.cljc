(ns maze.sidewinder
  (:require [maze.core :refer [full-grid size link-toward]]))

(defn close-out?
  "옆으로 진행하기를 멈추고 아래를 확장할 것인지?"
  [[rows cols] [r c]]
  (or (= c (dec cols))
      (and (< r (dec rows))
           (rand-nth [true false]))))

(deftype ^:private Sidewinder [output coords run]
  ISeqable
  (-seq [this] this)

  ISeq
  (-first [_] output)
  (-rest [_] (when-let [pos (first coords)]
               (let [run (conj run pos)
                     [pos dir new-run] (if (close-out? (:size output) pos)
                                         [(rand-nth run) :north []]
                                         [pos :east run])]
                 (Sidewinder. (-> output
                                  (update :grid link-toward pos dir)
                                  (assoc :frontier [pos dir]))
                              (rest coords)
                              new-run))
               #_(Sidewinder. (dissoc output :frontier)
                            nil
                            nil))))

(defn sidewinder [rows cols]
  (Sidewinder. {:size     [rows cols]
                :grid     (full-grid rows cols)
                :frontier nil}
               (->> (for [i (range rows) j (range cols)]
                      [i j])
                    (butlast))
               []))
