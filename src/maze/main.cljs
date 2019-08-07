(ns maze.main
  (:require [maze.core :as m]
            [maze.generator :refer [algorithms]]
            [reagent.core :as r]
            [cljs.core.async :refer [chan close! <! >! timeout]]
            ["p5" :as p5])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))


(defonce maze-size (r/atom [17 17]))
(defonce step-delay (r/atom 25))
(def ^:dynamic *cell-size* 20)

(defonce *state (atom {:ch-in    nil
                       :grid     nil
                       :frontier nil}))

(defn sleep [ms]
  (let [c (chan)]
    (js/setTimeout (fn [] (close! c)) ms)
    c))

(defn do-update [ch-in]
  (go-loop [m (<! ch-in)]
    (when m
      (<! (sleep @step-delay))
      (swap! *state assoc :grid (:grid m))
      (swap! *state assoc :frontier (:frontier m))
      (js/redraw)
      (recur (<! ch-in)))
    (swap! *state assoc :frontier nil)
    (js/redraw)))

(defn run-solo [algorithm]
  ; stop
  (when-let [c (:ch-in @*state)]
    (close! c))
  (when-let [c (:process @*state)]
    (close! c))
  (swap! *state assoc :grid nil)
  (swap! *state assoc :frontier nil)

  ; start
  (let [ch (chan)
        [_ seq-fn] (algorithms algorithm)]
    (swap! *state assoc :ch-in ch)
    (swap! *state assoc :process (do-update ch))
    (go-loop [mseq (apply seq-fn @maze-size)]
      (if-let [m (first mseq)]
        (do
          (>! ch m)
          (recur (rest mseq)))
        (close! ch)))))


;; UI & rendering

(defn setup []
  (js/createCanvas js/windowWidth js/windowHeight)
  (js/noLoop))

(defn window-resized []
  (js/resizeCanvas js/windowWidth js/windowHeight))

(defn draw-grid [grid cell-size]
  (let [[rows cols] (m/size grid)]
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
                            [:south [x1 y1 x2 y1]]]]
          (when-not (m/linked? grid [r c] dir)
            (apply js/line p1p2)))))))

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
  (let [{:keys [grid frontier]} @*state]
    (draw-grid grid *cell-size*)
    (when frontier
      (draw-frontier frontier *cell-size*))))


(doto js/window
  (aset "setup" setup)
  (aset "draw" draw)
  (aset "windowResized" window-resized))

(defonce fps (r/atom 0))
(defonce fps-updater (js/setInterval
                       #(reset! fps (js/frameRate)) 100))
(defn frame-display []
  [:div {:style {:position    "fixed"
                 :top         "0px"
                 :right       "0px"
                 :line-height "1"}}
   "FPS: " (.toFixed @fps 2)])

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

(defn delay-slider [min max]
  (let [val (r/atom 5)]
    (fn []
      [:input {:type      "range" :value @val :min min :max max
               :style     {:width "350px"}
               :on-change (fn [e]
                            (let [v (.. e -target -value)]
                              (reset! val v)
                              (reset! step-delay (* (int v) (int v)))))}])))

(defn controls []
  (fn []
    [:<>
     [:div
      [:div "Algorithms"]
      (for [[alg [text]] algorithms]
        ^{:key alg}
        [:button
         {:on-click #(run-solo alg)}
         text])]
     [:div
      [:div (str "Maze size: " @maze-size)]
      [size-adjust]]
     [:div
      [:div
       (str "Step delay: " @step-delay "ms")]
      [delay-slider 0 50]]]))


(defn ^:export run []
  (r/render [:div #_[frame-display] [controls]]
            (.getElementById js/document "app")))

(run)

;(stest/instrument)

#_(doseq [m (seq (aldous-broder 5 5))]
    (js/console.log (m/print (:grid m))
                    (clj->js (:frontier m))))
