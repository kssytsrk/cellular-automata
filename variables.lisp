(in-package #:ca)

(defparameter *window-height* 20)
(defparameter *window-width* 20)

(defparameter *world* nil
  "The environment of a cellular automaton.")

(defparameter *rulesets*
  '((1 . (((0 0 0) . 0)
          ((0 0 1) . 1)
          ((0 1 0) . 0)
          ((0 1 1) . 1)
          ((1 0 0) . 1)
          ((1 0 1) . 0)
          ((1 1 0) . 1)
          ((1 1 1) . 0)))))

(defparameter *colors*
  (list (cons 0 (sdl:color :r 0 :g 0 :b 0 :a 255))
        (cons 1 (sdl:color :r 255 :g 255 :b 255 :a 255))))
