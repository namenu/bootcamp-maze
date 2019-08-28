(ns maze.rendition
  (:require ["p5" :as p5]
            ["tone" :as Tone]
            [maze.core :as m]
            [maze.state :refer [*state]]))

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
(def cell-size 20)
(def wall-width 2)
(def padding 2)
(def ^:dynamic *style* :light)

(defn setup []
  (js/createCanvas js/windowWidth js/windowHeight)
  (js/noLoop))

(defn window-resized []
  (js/resizeCanvas js/windowWidth js/windowHeight))

(defn draw-grid [[rows cols] grid]
  (js/stroke 0)
  (js/strokeWeight wall-width)
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

(defn draw-grid2 [[rows cols] grid]
  (js/fill 0 0 100 100)
  (js/noStroke)

  (dorun
    (for [r (range rows) c (range cols)
          :let [x1 (* cell-size c)
                y1 (* cell-size r)
                x2 (* cell-size (inc c))
                y2 (* cell-size (inc r))]
          :when (not= (count (grid [r c])) 0)]
      (do
        (js/rect (+ x1 padding) (+ y1 padding) (- cell-size (* padding 2)) (- cell-size (* padding 2)))
        (doseq [[dir p1p2] [[:north [(+ x1 padding) (- y2 padding) (- cell-size (* padding 2)) padding]]
                            [:west [x1 (+ y1 padding) padding (- cell-size (* padding 2))]]
                            [:east [(- x2 padding) (+ y1 padding) padding (- cell-size (* padding 2))]]
                            [:south [(+ x1 padding) y1 (- cell-size (* padding 2)) padding]]]
                :when (m/linked? grid [r c] dir)]
          (apply js/rect p1p2))))))

(defn draw-distmap [[rows cols] distmap]
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

(defn draw-frontier [[[r c]]]
  (let [x1 (* cell-size c)
        y1 (* cell-size r)]
    (js/noStroke)
    (js/fill 255 0 0 128)
    (js/rect x1 y1 cell-size cell-size)))

(defn draw-frontier2 [[[r c]]]
  (let [x1 (* cell-size c)
        y1 (* cell-size r)]
    (js/fill 200 0 0)
    (js/rect (+ x1 padding) (+ y1 padding) (- cell-size (* padding 2)) (- cell-size (* padding 2)))))

(defmulti draw-maze :style)

(defmethod draw-maze :normal [{:keys [grid frontier distmap]}]
  (let [size (m/size grid)]
    (when distmap
      (draw-distmap size distmap))
    (draw-grid size grid)
    (when frontier
      (draw-frontier frontier))))

(defmethod draw-maze :light [{:keys [grid frontier distmap]}]
  (let [size (m/size grid)]
    (if distmap
      (draw-distmap size distmap)
      (draw-grid2 size grid))
    (when frontier
      (draw-frontier2 frontier))))

(defn draw []
  (js/background 255)

  (js/translate 10 10)
  (draw-maze (assoc (:output @*state) :style *style*)))

(defn redraw []
  (js/redraw))

(defn bootstrap []
  (doto js/window
    (aset "setup" setup)
    (aset "draw" draw)
    (aset "windowResized" window-resized)))
