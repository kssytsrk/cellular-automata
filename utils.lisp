;; -*-lisp-*-
;;
;; utils.lisp

(in-package #:ca)

(defun assoc-rh (item alist &key (test #'eql))
  "Returns the right side of assoc."
  (cdr (assoc item alist :test test)))

(defun color (number color-set)
  "Gets a color with number NUMBER from the color set COLOR-SET."
  (assoc-rh number color-set))

(defun decimal-to-base-n-list (number padding base)
  "Converts from a decimal number to a base-n number represented
as a list of digits."
  (map 'list
       (lambda (char)
         (parse-integer (string char) :radix base))
       (format nil
               (concatenate 'string
			    "~" (write-to-string base) ","
			    (write-to-string padding) ",'0R")
               number)))

(defun print-ca (ca w)
  "Prints CA's state to the repl" ; useful for debugging
  (loop for i across ca
        for n from 1
        do (format t "~d" i)
        if (eql (mod n w) 0)
          do (format t "~&")))

;;; only valid within with-pixel/s
(defmacro write-pixel (pix x y color)
  "Draws a pixel to the surface."
  `(sdl:write-pixel
    ,pix ,x ,y
    (apply #'sdl-cffi::sdl-map-rgba
	   (concatenate 'list
			(list (sdl-base:pixel-format surface-fp))
			(sdl:fp ,color)))))

(defun point-to-index (point bounds)
  "Converts from a point in the form of '(x y) to the index
of an array. Takes the point POINT and the dimensions of the
automaton (width ...)"
  (apply #'+ (first point) (mapcar #'*
                                 (rest bounds)
                                 (rest point))))

(defun %fix-bounds (n x)
  "Helper function for #'fix-bounds."
  (cond ((< x 0) (+ x n))
        ((> x (1- n)) (- x n))
        (t x)))

(defun fix-bounds (bounds cell)
  "Fixes the out-of-bounds coordinates."
  (mapcar #'%fix-bounds (reverse bounds) cell))

(defun get-all-cells (bounds)
  "Get all cells' coordinates in a rectangle between (0,0) and BOUNDS."
  (if bounds
      (loop for x from 0 below (first bounds)
            append (mapcar (lambda (list) (reverse (cons x list)))
                           (get-all-cells (rest bounds))))
      '(nil)))
