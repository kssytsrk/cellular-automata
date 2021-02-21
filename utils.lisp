(in-package #:ca)

(defun assoc-rh (item alist &key (test #'eql))
  (cdr (assoc item alist :test test)))

(defun color (number)
  (assoc-rh number *colors*))

(defun unpack-color (uint-color)
  (sdl:color :r (logand (ash uint-color -16) 255)
             :g (logand (ash uint-color -8) 255)
             :b (logand uint-color 255)))
