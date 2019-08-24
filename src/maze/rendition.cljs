(ns maze.rendition
  (:require ["p5" :as p5]
            ["tone" :as Tone]
            [maze.core :as m]
            [reagent.core :as r]))

(defonce *state (atom {:ch-in   nil
                       :mseq    nil
                       :output  nil}))
(defonce *history (r/atom []))

;; Tone.js
(def synth (let [synth (Tone/Synth.)]
             (set! (.. synth -oscillator -type) "sine4")
             (.toMaster synth)))

(defn play-note [[r c]]
  (let [size    (m/size (get-in @*state [:output :grid]))
        max-val (- (apply + size) 2)
        note    (js/map (+ r c) 0 max-val 110 1760)]
    (.triggerAttackRelease synth note "8n")))


;; p5.js
(def ^:dynamic *cell-size* 20)

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

(defn redraw []
  (js/redraw))

(defn bootstrap []
  (doto js/window
    (aset "setup" setup)
    (aset "draw" draw)
    (aset "windowResized" window-resized)))
