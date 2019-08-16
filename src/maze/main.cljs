(ns maze.main
  (:require [maze.generator :refer [algorithms]]
            [maze.graph :refer [dijkstra]]
            [maze.rendition :refer [*state bootstrap redraw play-note]]

            [reagent.core :as r]
            ["@smooth-ui/core-sc" :refer [Normalize Grid Row Col Box Button FormCheck Checkbox FormCheckLabel]]

            [cljs.core.async :refer [chan close! <! >!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))


(defonce maze-size (r/atom [17 17]))
(defonce sound (r/atom true))
(defonce step-delay (r/atom 25))

(defn sleep [ms]
  (let [c (chan)]
    (js/setTimeout (fn [] (close! c)) ms)
    c))

(defn do-update [ch-in]
  (go-loop [[cmd arg] (<! ch-in)]
    (when cmd
      (case cmd
        :init (let [[_ seq-fn] arg]
                (swap! *state assoc :mseq (apply seq-fn @maze-size)))

        :step (when-let [m (first (:mseq @*state))]
                (swap! *state assoc :output m)
                (swap! *state update :mseq rest)
                (redraw)
                (when (and @sound (:frontier m))
                  (play-note (first (:frontier m)))))

        :solve (let [start-pos arg]
                 (when-let [m (last (:mseq @*state))]
                   (swap! *state assoc :output m)
                   (swap! *state assoc :mseq nil))

                 (let [grid (get-in @*state [:output :grid])
                       distmap (dijkstra grid start-pos)]
                   (swap! *state assoc-in [:output :distmap] distmap)
                   (redraw)))

        :finish (do
                  (swap! *state assoc-in [:output :frontier] nil)
                  (redraw))

        "default")
      (recur (<! ch-in)))))

(defn run-solo [algorithm]
  ; stop
  (when-let [c (:ch-in @*state)]
    (close! c))
  (swap! *state assoc :output nil)

  ; start
  (let [ch (chan)]
    (swap! *state assoc :ch-in ch)
    (do-update ch)
    (go
      (>! ch [:init (algorithms algorithm)])
      
      ;; we must ensure (:mseq @*state) is created before go into the loop,
      ;; and injecting dummy event (:block) would do.
      (>! ch [:block])
      (loop []
        (when (and (:mseq @*state) (>! ch [:step]))
          (<! (sleep @step-delay))
          (recur)))
      (>! ch [:finish]))))

(defn solve [start-pos]
  (when-let [c (:ch-in @*state)]
    (go
      (>! c [:solve start-pos]))))

;; UI

(comment
  (defonce fps (r/atom 0))
  (defonce fps-updater (js/setInterval
                         #(reset! fps (js/frameRate)) 100))
  (defn frame-display []
    [:div {:style {:position    "fixed"
                   :top         "0px"
                   :right       "0px"
                   :line-height "1"}}
     "FPS: " (.toFixed @fps 2)]))

(defn size-adjust []
  (let [[rows cols] @maze-size
        change-fn (fn [k]
                    (fn [e]
                      (swap! maze-size assoc k (int (.. e -target -value)))))]
    [:<>
     [:input {:type      "number" :value rows :min 2
              :on-change (change-fn 0)}]
     [:input {:type      "number" :value cols :min 2
              :on-change (change-fn 1)}]]))

(defn sound-toggle []
  [:> FormCheck {:inline true}
   [:> Checkbox {:checked   @sound
                 :on-change (fn [e] (swap! sound not))}]
   [:> FormCheckLabel "Sound"]])

(defn delay-slider [min max]
  (let [val (r/atom 5)]
    (fn []
      [:input {:type      "range" :value @val :min min :max max
               :style     {:width "350px"}
               :on-change (fn [e]
                            (let [v (.. e -target -value)]
                              (reset! val v)
                              (reset! step-delay (* (int v) (int v)))))}])))

(defn starting-points [[r c]]
  {:lt [0 0]
   :rt [0 (dec c)]
   :lb [(dec r) 0]
   :rb [(dec r) (dec c)]
   :c  [(quot r 2) (quot c 2)]})

(defn dashboard []
  (fn []
    [:<>
     [:div "Algorithms"]
     [:> Box
      (for [[alg [text]] algorithms]
        ^{:key alg}
        [:> Button
         {:size     "sm"
          :mr       1
          :on-click #(run-solo alg)}
         text])]

     [:div
      [:div (str "Maze size: " @maze-size)]
      [size-adjust]
      [sound-toggle]]

     [:div
      [:div
       (str "Step delay: " @step-delay "ms")]
      [delay-slider 0 50]]

     (for [[name pos] (starting-points @maze-size)]
       ^{:key name}
       [:> Button
        {:variant  "dark"
         :size     "sm"
         :mr       "5px"
         :on-click (fn [_]
                     (solve pos))}
        name])]))


(defn ^:export run []
  (bootstrap)
  (r/render [:<>
             [:> Row
              [:> Col
               [dashboard]]]]
            (.getElementById js/document "app")))

(run)