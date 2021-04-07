;;;; -*-lisp-*-
;;;;
;;;; states.lisp

(in-package #:ca)

(defmacro state-fn (state-name)
  `(function ,(find-symbol (concatenate 'string
                                       (symbol-name state-name)
                                       "-STATE")
                          'ca)))

(defun empty-state ()
  (lambda (h w) (make-array (* h w) :initial-element 0)))

(defun dot-state (&rest points)
  (lambda (h w)
    (let ((array (funcall (empty-state) h w)))
      (dolist (point points)
        (setf (elt array (point-to-index point (list w h))) 1))
      array)))

(defun center-dot-state ()
  (lambda (h w)
    (funcall (dot-state (list (ceiling (/ h 2)) (ceiling (/ w 2)))) h w)))

(defun line-state (&rest lines)
  (lambda (h w)
    (let ((array (funcall (empty-state) h w)))
      (case w
        (0 nil)
        (1 (dolist (line lines)
             (loop for i from (first (first line)) to (first (second line))
                   do (setf (elt array i) 1))))
        (t (dolist (line lines)
             (loop with i = (first (first line))
                   and j = (second (first line))
                   until (and (eql i (first (second line)))
                              (eql j (second (second line))))
                   do (setf (elt array (point-to-index (list i j) (list w h))) 1)
                   unless (eql i (first (second line)))
                     do (incf i (if (< i (first (second line))) 1 -1))
                   unless (eql j (second (second line)))
                     do (incf j (if (< j (second (second line))) 1 -1))))))
      array)))

(defun random-dots-state (&optional (number 100))
  (lambda (h w)
    (let ((array (funcall (empty-state) h w)))
      (dotimes (i number)
        (setf (elt array (point-to-index (list (random w) (random h))
                                         (list w h)))
              1))
      array)))
