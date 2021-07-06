(ns maze.ui.main
  (:require [maze.generator :refer [algorithms]]
            [maze.graph :refer [dijkstra]]
            [maze.ui.state :refer [*state *history controls]]
            [maze.ui.rendition :refer [bootstrap redraw styles play-note]]
            [maze.ui.components.controls :refer [algorithm-buttons solve-buttons size-adjust style-changer delay-slider sound-toggle timeline-slider]]

            [reagent.dom :as rdom]
            ["@smooth-ui/core-sc" :as sc]

            [clojure.core.async :refer [chan close! <! >! go go-loop]]))

(defn sleep [ms]
  (let [c (chan)]
    (js/setTimeout (fn [] (close! c)) ms)
    c))

(defn do-update [ch-in]
  (go-loop [[cmd arg] (<! ch-in)]
    (when cmd
      (case cmd
        :init (let [{:keys [algorithm maze-size]} arg
                    [_ seq-fn] (algorithms algorithm)]
                (swap! *state assoc :mseq (apply seq-fn maze-size))
                (reset! *history [])
                (swap! controls assoc :seek-index nil))

        :step (when-let [m (first (:mseq @*state))]
                (swap! *state assoc :output m)
                (swap! *state update :mseq rest)
                (swap! *history conj m)
                (redraw)
                (when (and (:sound @controls) (:frontier m))
                  (play-note (first (:frontier m)))))

        :solve (let [start-pos arg]
                 (when-let [m (last (:mseq @*state))]
                   (swap! *state assoc :output m)
                   (swap! *state assoc :mseq nil))

                 (let [grid    (get-in @*state [:output :grid])
                       distmap (dijkstra grid start-pos)]
                   (swap! *state assoc-in [:output :distmap] distmap)
                   (redraw)))

        :finish (do
                  (swap! *state assoc-in [:output :frontier] nil)
                  (swap! *history conj (:output @*state))
                  (swap! controls assoc :seek-index (count @*history))
                  (redraw))

        "default")
      (recur (<! ch-in)))))

(defn process [opts]
  ; stop
  (when-let [c (:ch-in @*state)]
    (close! c))
  (swap! *state assoc :output nil)

  ; start
  (let [ch (chan)]
    (swap! *state assoc :ch-in ch)
    (do-update ch)
    (go
      (>! ch [:init opts])

      ;; we must ensure (:mseq @*state) is created before go into the loop,
      ;; and injecting dummy event (:block) would do.
      (>! ch [:block])
      (loop []
        (when (and (:mseq @*state) (>! ch [:step]))
          (<! (sleep (:step-delay @controls)))
          (recur)))
      (>! ch [:finish]))))

(defn solve [start-pos]
  (when-let [c (:ch-in @*state)]
    (go
      (>! c [:solve start-pos]))))

(defn time-travel [idx]
  (let [m (get @*history idx)]
    (swap! *state assoc :output m)
    (redraw)))

(defn ui []
  [:> sc/Row
   [:> sc/Col
    [:<>
     [:> sc/Typography {:as :h2} "Maze generator"]

     [:> sc/FormGroup
      [algorithm-buttons {:algorithms algorithms
                          :on-click   process}]
      [solve-buttons {:on-click solve}]]

     [:> sc/FormGroup
      [size-adjust]
      [style-changer {:styles    styles
                      :on-change redraw}]
      [sound-toggle]]

     [delay-slider 0 50]

     [timeline-slider {:seek-index (:seek-index @controls)
                       :length     (count @*history)
                       :on-change  time-travel}]]]])

(defn ^:dev/after-load start []
  (rdom/render [ui] (.getElementById js/document "app"))
  (redraw))

(defn init []
  (bootstrap)
  (rdom/render [ui] (.getElementById js/document "app")))
