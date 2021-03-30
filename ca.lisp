;;;; -*-lisp-*-
;;;;
;;;; ca.lisp

(in-package #:ca)

(defmacro neighbourhoods-fn (neighbourhood-name)
  `(function ,(find-symbol (concatenate 'string
                                        (symbol-name neighbourhood-name)
                                        "-NEIGHBOURHOODS")
                           'ca)))

(defun elementary-neighbourhood (n)
  (lambda (x)
    (let ((v0 (1- x))
          (v1 (1+ x))
          indices)
      (push x indices)
      (if (< v0 0)
          (push (+ n v0) indices)
          (push v0       indices))
      (if (> v1 (1- n))
          (push (- v1 n) indices)
          (push v1       indices))
      indices)))

(defun elementary-neighbourhoods (h w)
  (declare (ignore h))
  (mapcar (elementary-neighbourhood w) (loop for i from 0 below w collect i)))

(defun neumann-neighbourhood (n m)
  (lambda (x y)
    (let ((v0 (1+ x))
          (v1 (1+ y))
          (v2 (1- x))
          (v3 (1- y))
          indices)
      (push (+ x (* m y)) indices)
      (if (> v0 (1- m))
          (push (+ (- v0 m) (* m y)) indices)
          (push (+ v0 (* m y)) indices))
      (if (> v1 (1- n))
          (push (+ x (* m (- v1 n))) indices)
          (push (+ x (* m v1)) indices))
      (if (< v2 0)
          (push (+ (+ m v2) (* m y)) indices)
          (push (+ v2 (* m y)) indices))
      (if (< v3 0)
          (push (+ x (* m (+ n v3))) indices)
          (push (+ x (* m v3)) indices))
      (reverse indices))))

(defun neumann-neighbourhoods (n m)
  (mapcar (lambda (cell) (apply (neumann-neighbourhood n m) cell))
          (loop for y from 0 below n
                append (loop for x from 0 below m
                             collect (list x y)))))

(defun cell-neighbourhood (cells indices)
  (mapcar #'(lambda (index) (elt cells index))
          indices))

(defun next-state (neighbourhoods current-state rules)
  (loop for cell-nb in neighbourhoods
        for i from 0 below (length current-state)
        with result-vector = (make-array (length current-state))
        do (setf (elt result-vector i)
                 (transition-rule (cell-neighbourhood current-state cell-nb) rules))
        finally (return result-vector)))
