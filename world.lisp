(in-package #:ca)

(defstruct (world (:constructor %make-world))
  "A world structure."
  (name "default")
  (rows 10)
  (cols 10)
  (states '(:off :on))
  (array nil))

(defun make-world (&key
                   (name "default")
                   (rows 10) (cols 10)
                   (states '(:off :on))
                   (initial-element (first states)))
  (%make-world :name name
               :rows rows
               :cols cols
               :states states
               :array (make-array rows
                                  :initial-element
                                  (make-array cols
                                              :initial-element initial-element)
                                  :adjustable nil)))

(defparameter *world-matrix*
  (make-world :rows 30
              :cols 30
              :states '(0 1))
  "The environment of a cellular automaton.")

(setf (aref (aref (world-array *world-matrix*) 1) (/ (length (aref (world-array *world-matrix*) 1)) 2)) 1)

(defparameter *ruleset*
  '(((0 0 0) . 0)
    ((0 0 1) . 1)
    ((0 1 0) . 0)
    ((0 1 1) . 1)
    ((1 0 0) . 1)
    ((1 0 1) . 0)
    ((1 1 0) . 1)
    ((1 1 1) . 0)))

(defun calculate-world ()
  (loop for i from 0 to (- (length (world-array *world-matrix*)) 2)
        do (setf (aref (world-array *world-matrix*) (1+ i))
                 (calculate-next-generation (aref (world-array *world-matrix*) i))))
  *world-matrix*)

(defun calculate-next-generation (current-generation)
  (let ((next-generation (make-array (length current-generation))))
    (dotimes (i (length current-generation))
      (setf (aref next-generation i)
            (rest (assoc (list (handler-case
                                      (aref current-generation (1- i))
                                    (t () 0))
                                  (aref current-generation i)
                                  (handler-case
                                      (aref current-generation (1+ i))
                                    (t () 0)))
                            *ruleset* :test #'equal))))
    next-generation))

;(setf (aref *world-matrix* 5 5) :on)
