(ns maze.ui.components.controls
  (:require [reagent.core :as r]
            ["@smooth-ui/core-sc" :as sc]
            [maze.ui.state :refer [controls]]))
;; UI

(comment)
(defonce fps (r/atom 0))
(defonce fps-updater (js/setInterval
                       #(reset! fps (js/frameRate)) 100))
(defn frame-display []
  [:div {:style {:position    "fixed"
                 :top         "0px"
                 :right       "0px"
                 :line-height "1"}}
   "FPS: " (.toFixed @fps 2)])

(defn starting-points [[r c]]
  {:lt [0 0]
   :rt [0 (dec c)]
   :lb [(dec r) 0]
   :rb [(dec r) (dec c)]
   :c  [(quot r 2) (quot c 2)]})

(defn algorithm-buttons [{:keys [algorithms on-click]}]
  [:> sc/Box
   [:> sc/Label {:mr 1} "Algorithms: "]
   (for [[alg [text]] algorithms]
     ^{:key alg}
     [:> sc/Button
      {:size     "sm"
       :mr       1
       :on-click (fn [_] (on-click {:algorithm alg
                                    :maze-size (:maze-size @controls)}))}
      text])])

(defn solve-buttons [{:keys [on-click]}]
  [:> sc/Box {:mt 1}
   [:> sc/Label {:mr 1} "Solve: "]
   (for [[name pos] (starting-points (:maze-size @controls))]
     ^{:key name}
     [:> sc/Button
      {:variant  "dark"
       :size     "sm"
       :mr       "5px"
       :on-click #(on-click pos)}
      name])])

(defn size-adjust []
  (let [[rows cols] (:maze-size @controls)
        change-fn (fn [k]
                    (fn [e]
                      (swap! controls assoc-in [:maze-size k] (int (.. e -target -value)))))]
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
  (let [val (r/atom (js/Math.sqrt (:step-delay @controls)))]
    (fn []
      [:> sc/FormGroup
       [:> sc/Label (str "Step delay: " (:step-delay @controls) "ms")]
       [:> sc/Input {:control   true
                     :type      "range" :value @val :min min :max max
                     :width     "350px"
                     :on-change (fn [e]
                                  (let [v (.. e -target -value)]
                                    (reset! val v)
                                    (swap! controls assoc :step-delay (* (int v) (int v)))))}]])))

(defn style-changer [{:keys [styles on-change]}]
  [:> sc/Select
   {:defaultValue (:style @controls)
    :on-change    (fn [e]
                    (let [v (.. e -target -value)
                          style' (keyword v)]
                      (swap! controls assoc :style style')
                      (on-change style')))}
   (doall
     (for [option styles]
       ^{:key option}
       [:option {:value option} (keyword option)]))])

(defn sound-toggle []
  [:> sc/FormCheck {:inline true}
   [:> sc/Checkbox {:checked   (:sound @controls)
                    :on-change (fn [_] (swap! controls update :sound not))}]

   [:> sc/FormCheckLabel "Sound"]])

(defn timeline-slider [{:keys [seek-index max on-change]}]
  [:> sc/FormGroup
   [:> sc/Label (str "Playback: " (or seek-index "-") " / " max)]
   [:> sc/Input (merge {:control   true
                        :type      "range"
                        :style     {:width "350px"}
                        :on-change (fn [e]
                                     (let [v   (.. e -target -value)
                                           idx (js/parseInt v)]
                                       (on-change idx)
                                       (swap! controls assoc :seek-index idx)))}
                       (if seek-index
                         {:value seek-index :min 0 :max max}
                         {:disabled true :value 1 :min 0 :max 1}))]])

