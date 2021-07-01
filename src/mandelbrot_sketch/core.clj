(ns mandelbrot-sketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn paint-background [iter_condition x y]
  (if iter_condition
      (q/set-pixel x y (q/color 0))
      (q/set-pixel x y (q/color 255))))

(defn sq [n] (* n n))

(defn sum-squares [x y] (+ (sq x) (sq y)))

(defn x-func [x y c]
  (+ (- (sq x) (sq y)) c))

(defn y-func [x y c]
  (+ (* 2 x y) c))

(defn c-func [axis measure]
  (let [w q/screen-width
        mw (/ 4 w)]
    (* (- axis (/ measure 2)) mw)))

(defn c-re [y]
  (c-func y q/screen-width))

(defn c-im [x]
  (c-func x q/screen-height))


(defn setup [] nil)

(defn update-state [state] nil)

(defn draw-state [state]
  (paint-background true 250 250)
  )

(q/defsketch mandelbrot-sketch
  :title "Mandelbrot-Sketch"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
