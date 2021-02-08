(in-package #:ca)

(defparameter *window-height* 20)
(defparameter *window-width* 20)

(defparameter *world* nil
  "The environment of a cellular automaton.")

(defun get-ruleset (n)
  (when (and (>= n 0) (<= n 256))
    (let ((states (map 'list
                       (lambda (char)
                         (parse-integer (string char)))
                       (format nil "~8,'0b" n)))
          (patterns '((0 0 0) (0 0 1) (0 1 0) (0 1 1)
                      (1 0 0) (1 0 1) (1 1 0) (1 1 1))))
      (mapcar #'cons patterns states))))

(defparameter *ruleset* nil)

(defparameter *colors*
  (list (cons 0 (sdl:color :r 255 :g 255 :b 255 :a 255))
        (cons 1 (sdl:color :r 0 :g 0 :b 0 :a 255))))
