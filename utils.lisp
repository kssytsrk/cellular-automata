(in-package #:ca)

(defun assoc-rh (item alist &key (test #'eql))
  (cdr (assoc item alist :test test)))

(defun get-color (number)
  (assoc-rh number *colors*))
