(ns maze.main
  (:require [maze.generator :refer [algorithms]]
            [maze.graph :refer [dijkstra]]
            [maze.state :refer [*state *history]]
            [maze.rendition :refer [bootstrap redraw play-note]]

            [reagent.core :as r]
            ["@smooth-ui/core-sc" :as sc]

            [cljs.core.async :refer [chan close! <! >!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))


(defonce maze-size (r/atom [17 17]))
(defonce sound (r/atom true))
(defonce step-delay (r/atom 25))
(defonce seek-index (r/atom nil))

(defn sleep [ms]
  (let [c (chan)]
    (js/setTimeout (fn [] (close! c)) ms)
    c))

(defn do-update [ch-in]
  (go-loop [[cmd arg] (<! ch-in)]
    (when cmd
      (case cmd
        :init (let [[_ seq-fn] arg]
                (swap! *state assoc :mseq (apply seq-fn @maze-size))
                (reset! *history [])
                (reset! seek-index nil))

        :step (when-let [m (first (:mseq @*state))]
                (swap! *state assoc :output m)
                (swap! *state update :mseq rest)
                (swap! *history conj m)
                (redraw)
                (when (and @sound (:frontier m))
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
                  (reset! seek-index (dec (count @*history)))
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

(defn time-travel [idx]
  (reset! seek-index idx)
  (let [m (get @*history idx)]
    (swap! *state assoc :output m))
  (redraw))

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
     [:span "Maze size: "]
     [:> sc/Input {:type      "number"
                   :width     50
                   :value     rows
                   :min       2
                   :on-change (change-fn 0)}]
     [:> sc/Input {:type      "number"
                   :width     50
                   :value     cols
                   :min       2
                   :on-change (change-fn 1)}]]))

(defn delay-slider [min max]
  (let [val (r/atom (js/Math.sqrt @step-delay))]
    (fn []
      [:> sc/Input {:control   true
                    :type      "range" :value @val :min min :max max
                    :width     "350px"
                    :on-change (fn [e]
                                 (let [v (.. e -target -value)]
                                   (reset! val v)
                                   (reset! step-delay (* (int v) (int v)))))}])))

(defn sound-toggle []
  [:> sc/FormCheck {:inline true}
   [:> sc/Checkbox {:checked   @sound
                    :on-change (fn [_] (swap! sound not))}]
   [:> sc/FormCheckLabel "Sound"]])

(defn starting-points [[r c]]
  {:lt [0 0]
   :rt [0 (dec c)]
   :lb [(dec r) 0]
   :rb [(dec r) (dec c)]
   :c  [(quot r 2) (quot c 2)]})

(defn timeline-slider []
  [:> sc/FormGroup
   [:> sc/Label "Playback:"]
   [:> sc/Input (merge {:control   true
                        :type      "range"
                        :style     {:width "350px"}
                        :on-change (fn [e]
                                     (let [v (.. e -target -value)]
                                       (time-travel (js/parseInt v))))}
                       (if @seek-index
                         {:value @seek-index :min 0 :max (dec (count @*history))}
                         {:disabled true :value 1 :min 0 :max 1}))]])

(defn dashboard []
  (fn []
    [:<>
     [:> sc/Typography {:as :h2} "Maze generator"]

     [:> sc/FormGroup
      [:> sc/Box
       [:> sc/Label {:mr 1} "Algorithms: "]
       (for [[alg [text]] algorithms]
         ^{:key alg}
         [:> sc/Button
          {:size     "sm"
           :mr       1
           :on-click #(run-solo alg)}
          text])]

      [:> sc/Box {:mt 1}
       [:> sc/Label {:mr 1} "Solve: "]
       (for [[name pos] (starting-points @maze-size)]
         ^{:key name}
         [:> sc/Button
          {:variant  "dark"
           :size     "sm"
           :mr       "5px"
           :on-click (fn [_]
                       (solve pos))}
          name])]]

     [:> sc/FormGroup
      [size-adjust]
      [sound-toggle]]

     [:> sc/FormGroup
      [:> sc/Label
       (str "Step delay: " @step-delay "ms")]
      [delay-slider 0 50]]

     [timeline-slider]]))


(defn ^:export run []
  (r/render [:<>
             [:> sc/Row
              [:> sc/Col
               [dashboard]]]]
            (.getElementById js/document "app")))

(defn ^:export init []
  (bootstrap)
  (run))

(defn ^:dev/after-load start []
  (run)
  (redraw))
