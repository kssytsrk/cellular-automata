;;;; -*-lisp-*-
;;;;
;;;; ca.lisp

(in-package #:ca)

(defun neighbourhood-variable (nb-name)
  (eval (find-symbol (concatenate 'string
                                  "*"
                                  (symbol-name nb-name)
                                  "-NB*")
                     'ca)))

(defun cell-neighbourhoods (neighbourhoods bounds)
  (mapcar (lambda (cell)
            (mapcar (lambda (neighbourhood)
                      (let ((res (fix-bounds bounds
                                             (mapcar #'+ cell neighbourhood))))
                        (apply #'+ (first res) (mapcar #'*
                                                       (rest bounds)
                                                       (rest res)))))
                    neighbourhoods))
          (get-all-cells bounds)))

(defun get-all-cells (bounds)
  (if bounds
      (loop for y from 0 below (first bounds)
            append (mapcar (lambda (list) (append (list y) list))
                           (get-all-cells (rest bounds))))
      '(nil)))

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

(defun %fix-bounds (n x)
  (cond ((< x 0) (+ x n))
        ((> x (1- n)) (- x n))
        (t x)))

(defun fix-bounds (bounds cell)
  (mapcar #'%fix-bounds (reverse bounds) cell))
