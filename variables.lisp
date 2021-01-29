(in-package #:ca)

(defparameter *screen-width* 200)
(defparameter *screen-height* 200)

(defparameter *world-matrix* nil
  "The environment of a cellular automaton.")

(defparameter *ruleset*
  '(((0 0 0) . 0)
    ((0 0 1) . 1)
    ((0 1 0) . 0)
    ((0 1 1) . 1)
    ((1 0 0) . 1)
    ((1 0 1) . 0)
    ((1 1 0) . 1)
    ((1 1 1) . 0)))

(defparameter *bg-color* '(0 0 0 255))
(defparameter *fg-color* '(255 255 255 255))
