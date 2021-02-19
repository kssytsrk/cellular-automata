(in-package #:ca)

(defvar *window-width* 50)
(defvar *window-height* 50)

(defparameter *ruleset* nil)

(defparameter *neighbourhood* nil)

(defparameter *colors*
  (list (cons 0 (sdl:color :r 255 :g 255 :b 255 :a 255))
        (cons 1 (sdl:color :r 0 :g 0 :b 0 :a 255))))

(defun start (&key (h 500) (w 500)
                (ruleset 1) (neighbourhood :1d) p1 p2)
  "Start the program."
  (setf *window-width* w)
  (setf *window-height* h)
  (setf *neighbourhood* neighbourhood)

  (if (or (and (not (realp ruleset))
               (setf *ruleset* ruleset))
          (setf *ruleset* (get-ruleset ruleset)))
      (initialize p1 p2)
      (format t "Wrong ruleset number input.")))
