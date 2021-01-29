(in-package #:ca)

(defun start ()
  "Start the program."
  (let ((world (make-world :rows *screen-height*
                           :cols *screen-width*
                           :states '(0 1))))
    (setf (world-aref world 0 (/ (world-cols world) 2)) 1)
    (calculate-world world *ruleset*)
    (initialize *screen-height* *screen-width* world)))
