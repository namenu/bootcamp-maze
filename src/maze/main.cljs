(ns maze.main
  (:require [maze.core :as m]
            [maze.generator :refer [algorithms]]
            [maze.graph :refer [dijkstra]]

            [reagent.core :as r]
            ["@smooth-ui/core-sc" :refer [Normalize Grid Row Col Box Button FormCheck Checkbox FormCheckLabel]]

            [cljs.core.async :refer [chan close! <! >! timeout]]
            ["p5" :as p5]
            ["tone" :as Tone])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))


(defonce maze-size (r/atom [17 17]))
(defonce step-delay (r/atom 25))
(defonce sound (r/atom true))
(def ^:dynamic *cell-size* 20)

(defonce synth (.toMaster (Tone/Synth.)))

(defonce *state (atom {:ch-in    nil
                       :mseq     nil
                       :output   nil}))

(defn sleep [ms]
  (let [c (chan)]
    (js/setTimeout (fn [] (close! c)) ms)
    c))

(defn play-note [[r c]]
  (let [max-val (- (apply + @maze-size) 2)
        note    (js/map (+ r c) 0 max-val 110 1760)]
    (.triggerAttackRelease synth note "8n")))

(defn do-update [ch-in]
  (go-loop [[cmd arg] (<! ch-in)]
    (when cmd
      (case cmd
        :init (let [[_ seq-fn] arg]
                (swap! *state assoc :mseq (apply seq-fn @maze-size)))

        :step (when-let [m (first (:mseq @*state))]
                (swap! *state assoc :output m)
                (swap! *state update :mseq rest)
                (js/redraw)
                (when (and @sound (:frontier m))
                  (play-note (first (:frontier m))))
                (<! (sleep @step-delay)))

        :solve (let [start-pos arg]
                 (when-let [m (last (:mseq @*state))]
                   (swap! *state assoc :output m)
                   (swap! *state assoc :mseq nil))

                 (let [grid (get-in @*state [:output :grid])
                       distmap (dijkstra grid start-pos)]
                   (swap! *state assoc-in [:output :distmap] distmap)
                   (js/redraw)))

        :finish (do
                  (swap! *state assoc-in [:output :frontier] nil)
                  (js/redraw))

        "default")
      (recur (<! ch-in)))))

(defn run-solo [algorithm]
  ; stop
  (when-let [c (:ch-in @*state)]
    (close! c))
  (when-let [c (:process @*state)]
    (close! c))
  (swap! *state assoc :output nil)

  ; start
  (let [ch (chan)]
    (swap! *state assoc :ch-in ch)
    (swap! *state assoc :process (do-update ch))
    (go
      (>! ch [:init (algorithms algorithm)])
      (>! ch [:block])
      (loop []
        (when (:mseq @*state)
          (if (>! ch [:step])
            (recur))))
      (>! ch [:finish]))))

(defn solve [start-pos]
  (when-let [c (:ch-in @*state)]
    (go
      (>! c [:solve start-pos]))))

;; UI & rendering

(defn setup []
  (js/createCanvas js/windowWidth js/windowHeight)
  (js/noLoop))

(defn window-resized []
  (js/resizeCanvas js/windowWidth js/windowHeight))

(defn draw-grid [[rows cols] grid cell-size]
  (js/stroke 0)
  (js/strokeWeight 1)
  (dorun
    (for [r (range rows) c (range cols)
          :let [x1 (* cell-size c)
                y1 (* cell-size r)
                x2 (* cell-size (inc c))
                y2 (* cell-size (inc r))]]
      (doseq [[dir p1p2] [[:north [x1 y2 x2 y2]]
                          [:west [x1 y1 x1 y2]]
                          [:east [x2 y1 x2 y2]]
                          [:south [x1 y1 x2 y1]]]
              :when (not (m/linked? grid [r c] dir))]
        (apply js/line p1p2)))))

(defn draw-distmap [[rows cols] distmap cell-size]
  (let [max-depth (apply max (vals distmap))]
    (js/noStroke)
    (dorun
      (for [r (range rows) c (range cols)
            :let [x     (* cell-size c)
                  y     (* cell-size r)
                  dist  (distmap [r c])
                  color (js/map dist 0 max-depth 0 255)]]
        (do
          (js/fill 255 60 60 color)
          (js/rect x y cell-size cell-size))))))

(defn draw-frontier [[[r c] dir] cell-size]
  (let [x1   (* cell-size c)
        y1   (* cell-size r)
        x2   (* cell-size (inc c))
        y2   (* cell-size (inc r))
        p1p2 ({:north [x1 y2 x2 y2]
               :west  [x1 y1 x1 y2]
               :east  [x2 y1 x2 y2]
               :south [x1 y1 x2 y1]}
              dir)]
    (js/fill 255 0 0 128)
    (js/rect x1 y1 cell-size cell-size)
    #_(js/stroke 255 0 0)
    #_(js/strokeWeight 2)
    #_(apply js/line p1p2)))

(defn draw []
  (js/background 255)

  (js/translate 10 10)
  (let [{:keys [grid frontier distmap]} (:output @*state)
        size (m/size grid)]
    (when distmap
      (draw-distmap size distmap *cell-size*))
    (draw-grid size grid *cell-size*)
    (when frontier
      (draw-frontier frontier *cell-size*))))


(doto js/window
  (aset "setup" setup)
  (aset "draw" draw)
  (aset "windowResized" window-resized))

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
                     (solve pos)
                     (js/redraw))}
        name])]))


(defn ^:export run []
  (r/render [:<>
             [:> Row
              [:> Col
               [dashboard]]]]
            (.getElementById js/document "app")))

(run)
