(ns maze.state
  (:require [reagent.core :as r]))

(defonce *state (atom {:ch-in  nil
                       :mseq   nil
                       :output nil}))
(defonce *history (r/atom []))

;; UI state
(defonce maze-size (r/atom [17 17]))
(defonce sound (r/atom true))
(defonce step-delay (r/atom 25))
(defonce seek-index (r/atom nil))
(defonce style (r/atom :outline))
