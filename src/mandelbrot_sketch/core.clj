(ns mandelbrot-sketch.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(set! *warn-on-reflection* true)


(defn coerce-unformattable-types [args]
  (map (fn [x]
         (cond (instance? clojure.lang.BigInt x) (biginteger x)
               (instance? clojure.lang.Ratio x) (double x)
               :else x))
       args))

(defn format-plus [fmt & args]
  (apply format fmt (coerce-unformattable-types args)))
;; ^ for debug

(def max-iter 1000)

(def screen-width 500)
(def screen-height 500)

(defn paint-background [iter_condition x y]
  (if iter_condition
      (q/set-pixel x y (q/color 0))
      (q/set-pixel x y (q/color 255))))

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

(defn update-state [state] nil)

(defn recur-values [x y iterations row col]
  (if (and (< (sum-squares x y) 4) (< iterations max-iter))
    (recur-values
     (x-func x y (c-re col)) (y-func x y (c-im row)) (+ iterations 1) row col)
    iterations))


(defn inc-int ^long [^long n] (inc n))

(defn draw-state [state]
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
                (swap!  iterations inc-int))
            ;(println (format-plus "row:%d col:%d iter:%d" row col @iterations))
            (paint-background (< @iterations max-iter) row col)))))


(q/defsketch mandelbrot-sketch
  :title "Mandelbrot-Sketch"
  :size [screen-width screen-height]
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

(defn -main [& args])
