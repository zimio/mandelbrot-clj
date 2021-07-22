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

(defn x-func ^double [^double x
                      ^double y
                      ^double c]
  (+ (- (sq x) (sq y)) c))

(defn y-func ^double [^double y
                      ^double x
                      ^double c]
  (+ (* 2 x y) c))

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

(defn recur-values [x y iterations row col]
  (if (and (< (sum-squares x y) 4) (< iterations max-iter))
    (recur-values
     (x-func x y (c-re col)) (y-func x y (c-im row)) (+ iterations 1) row col)
    iterations))


(defn inc-long ^long [^long n] (inc n))

(defn draw-naive [state]
    (dotimes [row screen-width]
        (dotimes [col screen-height]
            (let [iterations   (atom 0)
                    x          (atom 0)
                    x_temp     (atom 0)
                    y          (atom 0)]

                (while (and (< (sum-squares @x @y) 4) (< @iterations max-iter))
                    (swap!  x_temp x-func @y (c-re col))
                    (swap!  y      y-func @x (c-im row))
                    (reset! x      @x_temp            )
                    (swap!  iterations inc-long))
                (paint-background (< @iterations max-iter) row col)))))

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
                        (let [iterations    (atom 0)
                                x           (atom 0)
                                y           (atom 0)
                                x2          (atom 0)
                                y2          (atom 0)]

                            (while (and (< (+ @x2 @y2) 4) (< @iterations max-iter))
                                (reset! y (y-func-op @x @y row))
                                (reset! x (x-func-op @x2 @y2 col))
                                (reset! x2 (sq @x))
                                (reset! y2 (sq @y))
                                (swap!  iterations inc-long))
                            (aset-int pixels (+ row (* col screen-width)) (paint-background (< @iterations max-iter) row col)))))
        (q/update-pixels))))


(q/defsketch mandelbrot-sketch
  :title "Mandelbrot-Sketch"
  :size [screen-width screen-height]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update is called on each iteration before draw.
  :update update-sketch
  :draw draw-op
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

(defn -main [& args])
