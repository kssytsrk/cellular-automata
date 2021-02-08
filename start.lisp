(in-package #:ca)

(defun start (&key (h 500) (w 500)
                (ruleset 1))
  "Start the program."
  (setf *window-width* w)
  (setf *window-height* h)
  (setf *world* (make-world :rows h
                            :cols w
                            :states '(0 1)))
  (setf (world-aref *world* 0 (/ (world-cols *world*) 2)) 1)

  (if (not (setf *ruleset* (get-ruleset ruleset)))
      (format t "Wrong ruleset number input.")
      (progn
        (calculate-world *world* *ruleset*)
        (initialize))))
