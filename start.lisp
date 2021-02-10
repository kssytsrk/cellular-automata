(in-package #:ca)

(defun start (&key (h 500) (w 500)
                (ruleset 1) (d 1))
  "Start the program."
  (setf *window-width* w)
  (setf *window-height* h)
  (if (not (setf *ruleset* (get-ruleset ruleset d)))
      (format t "Wrong ruleset number input.")
      (initialize d)))
