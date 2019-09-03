(ns maze.state
  (:require [reagent.core :as r]))

(defonce *state (atom {:ch-in  nil
                       :mseq   nil
                       :output nil}))
(defonce *history (r/atom []))

;; UI state
(defonce controls (r/atom
                    {:maze-size  [17 17]
                     :sound      true
                     :step-delay 25
                     :seek-index nil
                     :style      :outline}))
