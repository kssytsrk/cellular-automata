(in-package #:ca)

(defvar *window-width* 50)
(defvar *window-height* 50)

(defun start (&key (h 500) (w 500)
                (ruleset 1) (d 1) p1 p2)
  "Start the program."
  (setf *window-width* w)
  (setf *window-height* h)

  (if (or (and (not (realp ruleset))
               (setf *ruleset* ruleset))
          (setf *ruleset* (get-ruleset ruleset d)))
      (initialize d p1 p2)
      (format t "Wrong ruleset number input.")))
