;;;; -*-lisp-*-
;;;;
;;;; start.lisp

(in-package #:ca)

(defvar *window-width* 50)
(defvar *window-height* 50)

(defparameter *ruleset* nil)

(defparameter *neighbourhood* nil)
(defparameter *totalistic* nil)

(defparameter *colors*
  (list (cons 0  (sdl:color :r 48  :g 48  :b 48  :a 255))
        (cons 1  (sdl:color :r 255 :g 0   :b 0   :a 255))
        (cons 2  (sdl:color :r 255 :g 125 :b 0   :a 255))
        (cons 3  (sdl:color :r 255 :g 175 :b 0   :a 255))
        (cons 4  (sdl:color :r 251 :g 255 :b 0   :a 255))
        (cons 5  (sdl:color :r 255 :g 200 :b 75  :a 255))
        (cons 6  (sdl:color :r 255 :g 150 :b 25  :a 255))
        (cons 7  (sdl:color :r 255 :g 255 :b 100 :a 255))
        (cons 8  (sdl:color :r 255 :g 250 :b 125 :a 255))
        (cons 9  (sdl:color :r 0   :g 255 :b 128 :a 255))
        (cons 10 (sdl:color :r 33  :g 215 :b 215 :a 255))
        (cons 11 (sdl:color :r 255 :g 255 :b 128 :a 255))
        (cons 12 (sdl:color :r 255 :g 128 :b 64  :a 255))
        (cons 13 (sdl:color :r 36  :g 200 :b 36  :a 255))
        (cons 14 (sdl:color :r 106 :g 106 :b 255 :a 255))
        (cons 15 (sdl:color :r 106 :g 255 :b 106 :a 255))
        (cons 16 (sdl:color :r 139 :g 139 :b 255 :a 255))
        (cons 17 (sdl:color :r 73  :g 255 :b 73  :a 255))
        (cons 18 (sdl:color :r 122 :g 122 :b 255 :a 255))
        (cons 19 (sdl:color :r 27  :g 176 :b 27  :a 255))
        (cons 20 (sdl:color :r 89  :g 89  :b 255 :a 255))
        (cons 21 (sdl:color :r 191 :g 73  :b 255 :a 255))
        (cons 22 (sdl:color :r 255 :g 56  :b 56  :a 255))
        (cons 23 (sdl:color :r 203 :g 106 :b 255 :a 255))
        (cons 24 (sdl:color :r 255 :g 89  :b 89  :a 255))
        (cons 25 (sdl:color :r 197 :g 89  :b 255 :a 255))
        (cons 26 (sdl:color :r 255 :g 73  :b 73  :a 255))
        (cons 27 (sdl:color :r 185 :g 56  :b 255 :a 255))
        (cons 28 (sdl:color :r 235 :g 36  :b 36  :a 255))))

(defun start (&key (h 500) (w 500)
                (ruleset 1) (neighbourhood :elementary) (totalistic nil) shapes)
  "Start the program."
  (setf *window-width* w)
  (setf *window-height* h)
  (setf *neighbourhood* neighbourhood)
  (setf *totalistic* totalistic)

  (if (or (and (not (realp ruleset))
               (setf *ruleset* ruleset))
          (setf *ruleset* (if *totalistic*
			      (totalistic-ruleset ruleset)
			      (ruleset ruleset))))
      (initialize shapes)
      (format t "Wrong ruleset number input.")))
