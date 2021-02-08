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
  (calculate-world *world* (cdr (assoc ruleset *rulesets*)))
  (initialize))
