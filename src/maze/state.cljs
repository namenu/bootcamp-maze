(ns maze.state
  (:require [reagent.core :as r]))

(defonce *state (atom {:ch-in   nil
                       :mseq    nil
                       :output  nil}))
(defonce *history (r/atom []))
