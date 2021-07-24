(ns mandelbrot-sketch.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(set! *warn-on-reflection* true)

(def max-iter 1000)
(def screen-width 500)
(def screen-height 500)

(defn paint-background [iter_condition x y]
  (if iter_condition
      (q/color 0)
      (q/color 255)))

(defn sq ^double [^double n] (* n n))

(defn sum-squares ^double [^double x
                           ^double y] (+ (sq x) (sq y)))

(defn c-func ^double [^long axis
                      ^long measure]
  (let [w screen-width
        mw (/ 4 w)]
    (* (- axis (/ measure 2)) mw)))

(defn c-re ^double [^double y]
  (c-func y screen-width))

(defn c-im ^double [^double x]
  (c-func x screen-height))


(defn setup []
  (q/background 100)
  (q/no-loop))

(defn update-sketch [state] nil)

(defn y-func-op ^double [^double x
                         ^double y
                         ^long row]
    (+ (* 2 x y )(c-im row)))

(defn x-func-op ^double [^double x2
                         ^double y2
                         ^long col]
    (+ (- x2 y2) (c-re col)))

(defn draw-op [state]
  (time
    (let [pixels (q/pixels)]
        (dotimes [row screen-width]
                    (dotimes [col screen-height]
                      (let [iterations
                        (loop [iter 0
                               y    0.0
                               x    0.0
                               x2   0.0
                               y2   0.0]
                            (if (and (< (+ x2 y2) 4) (< iter max-iter))
                              (let [new_y (y-func-op x y row)
                                    new_x (x-func-op x2 y2 col)]
                                (recur (inc iter) new_y new_x (sq new_x) (sq new_y)))
                                iter))]
                        (aset-int pixels (+ row (* col screen-width)) (paint-background (< iterations max-iter) row col)))))
        (q/update-pixels))))


(q/defsketch mandelbrot-sketch
  :title "Mandelbrot-Sketch"
  :size [screen-width screen-height]
  ; setup function called only once, during sketch initialization.
  :setup setup
  :draw draw-op
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

(defn -main [& args])
