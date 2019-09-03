(ns maze.rendition
  (:require ["p5" :as p5]
            ["tone" :as Tone]
            [maze.core :as m]
            [maze.state :refer [*state controls]]))

;; Tone.js
(def oscillator "triangle" #_"sine4")

(defonce synth (atom nil))

(defn make-synth []
  (let [synth (Tone/Synth.)]
    (set! (.. synth -oscillator -type) oscillator)
    (.toMaster synth)
    synth))

(defn play-note [[r c]]
  (let [size    (m/size (get-in @*state [:output :grid]))
        max-val (- (apply + size) 2)
        note    (js/map (+ r c) 0 max-val 110 1760)]
    (.triggerAttackRelease @synth note "8n")))


;; p5.js
(def cell-size 20)
(def wall-width 2)
(def padding 4)

(def styles [:normal :outline :light :heavy])

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

(defn h-line [x y l]
  (js/line x y (+ x l) y))

(defn v-line [x y l]
  (js/line x y x (+ y l)))

(defn draw-grid3 [[rows cols] grid]
  (js/fill 0 0 100 100)
  (js/stroke 0)
  (js/strokeWeight wall-width)

  (letfn [(draw-passage [[x1 y1 x2 y2] dir]
            (case dir
              :north (do
                       (v-line (+ x1 padding) (- y2 padding) padding)
                       (v-line (- x2 padding) (- y2 padding) padding))
              :west (do
                      (h-line x1 (+ y1 padding) padding)
                      (h-line x1 (- y2 padding) padding))
              :east (do
                      (h-line (- x2 padding) (+ y1 padding) padding)
                      (h-line (- x2 padding) (- y2 padding) padding))
              :south (do
                       (v-line (+ x1 padding) y1 padding)
                       (v-line (- x2 padding) y1 padding))))
          (draw-wall [[x1 y1 x2 y2] dir]
            (let [l (- cell-size (* padding 2))]
              (case dir
                :north (h-line (+ x1 padding) (- y2 padding) l)
                :south (h-line (+ x1 padding) (+ y1 padding) l)
                :west (v-line (+ x1 padding) (+ y1 padding) l)
                :east (v-line (- x2 padding) (+ y1 padding) l))))]
    (dorun
      (for [r (range rows) c (range cols)
            :let [x1 (* cell-size c)
                  y1 (* cell-size r)
                  x2 (* cell-size (inc c))
                  y2 (* cell-size (inc r))]
            :when (not= (count (grid [r c])) 0)]
        (doseq [dir [:west :east :north :south]]
          (if (m/linked? grid [r c] dir)
            (draw-passage [x1 y1 x2 y2] dir)
            (draw-wall [x1 y1 x2 y2] dir)))))))

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

(defmethod draw-maze :outline [{:keys [grid frontier distmap]}]
  (let [size (m/size grid)]
    (if distmap
      (draw-distmap size distmap)
      (draw-grid3 size grid))
    (when frontier
      (draw-frontier2 frontier))))

(defmethod draw-maze :heavy [{:keys [grid frontier distmap]}]
  (let [size (m/size grid)]
    (if distmap
      (draw-distmap size distmap)
      (do
        (draw-grid2 size grid)
        (draw-grid3 size grid)))
    (when frontier
      (draw-frontier2 frontier))))

(defn draw []
  (js/background 255)

  (js/translate 10 10)
  (draw-maze (assoc (:output @*state) :style (:style @controls))))

(defn redraw []
  (js/redraw))

(defn bootstrap []
  (doto js/window
    (aset "setup" setup)
    (aset "draw" draw)
    (aset "windowResized" window-resized))

  (swap! synth make-synth))
