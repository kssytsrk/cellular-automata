;;;; -*-lisp-*-
;;;;
;;;; ca.lisp

(in-package #:ca)

(defun 1d-neighbourhood (n &optional (nc 3))
  (lambda (x)
    (let ((start (- x (/ (- nc 1) 2)))
          (end   (+ x (/ (- nc 1) 2)))
          (list  (make-list n :initial-element 0))
          out-of-bounds)
      (when (< start 0)
        (fill list 1 :start (+ n start))
        (setf out-of-bounds (cons 'x-start (+ n start)))
        (setf start 0))
      (when (> end (1- n))
        (fill list 1 :end (- end (1- n)))
        (setf out-of-bounds (cons 'x-end (- end (1- n))))
        (setf end (1- n)))
      (cons (fill list 1 :start start :end (1+ end)) out-of-bounds))))

(defun 1d-neighbourhoods (n &optional (nc 3))
  (mapcar (1d-neighbourhood n nc) (loop for i from 0 below n collect i)))

(defun neumann-neighbourhood (n m)
  (lambda (x y)
    (let ((v0 (1+ x))
          (v1 (1+ y))
          (v2 (1- x))
          (v3 (1- y))
          indices)
      (push (cons 'v (+ x (* m y))) indices)
      (if (> v0 (1- m))
          (push (cons 'v0 (+ (- v0 m) (* m y))) indices)
          (push (cons 'v0 (+ v0 (* m y))) indices))
      (if (> v1 (1- n))
          (push (cons 'v1 (+ x (* m (- v1 n)))) indices)
          (push (cons 'v1 (+ x (* m v1))) indices))
      (if (< v2 0)
          (push (cons 'v2 (+ (+ m v2) (* m y))) indices)
          (push (cons 'v2 (+ v2 (* m y))) indices))
      (if (< v3 0)
          (push (cons 'v3 (+ x (* m (+ n v3)))) indices)
          (push (cons 'v3 (+ x (* m v3))) indices))
      (reverse indices))))

(defun neumann-neighbourhoods (n m)
  (mapcar (lambda (cell) (apply (neumann-neighbourhood n m) cell))
          (loop for y from 0 below n
                append (loop for x from 0 below m
                             collect (list x y)))))

(defun & (bm v2)
  (remove-if #'null
             (fix-bounds
              (cdr bm)
              (map 'list
                   (lambda (a1 a2) (and (eql 1 a1) a2))
                   (car bm) v2))))

(defun fix-bounds (out-of-bounds list)
  (let ((res list)
        (y-start (assoc 'y-start out-of-bounds))
        (x-start (assoc 'x-start out-of-bounds))
        (y-end (assoc 'y-end out-of-bounds))
        (x-end (assoc 'x-end out-of-bounds)))
    (when x-start
      (setf res
            (append (list (elt res (cdr x-start)))
                    (subseq res 0 (cdr x-start))
                    (subseq res (1+ (cdr x-start)))))
      (if (and y-start (< (cdr y-start) (cdr x-start)))
          (incf (cdr y-start)))
      (if y-end
          (incf (cdr y-end))))
    (when x-end
      (setf res
            (append (subseq res 0 (cdr x-end))
                    (subseq res (cdr x-end))
                    (list (elt res (cdr x-end)))))
      (if y-start
          (decf (cdr y-start)))
      (if (and y-end (> (cdr y-end) (cdr x-end)))
          (decf (cdr y-end))))
    (when y-start
      (setf res
            (append (list (elt res (cdr y-start)))
                    (subseq res 0 (cdr y-start))
                    (subseq res (1+ (cdr y-start))))))
    (when y-end
      (setf res
            (append (subseq res 0 (cdr y-end))
                    (subseq res (cdr y-end))
                    (list (elt res (cdr y-end))))))
    res))

(defun neumann-order (cells indices)
  (list (elt cells (assoc-rh 'v  indices))
        (elt cells (assoc-rh 'v0 indices))
        (elt cells (assoc-rh 'v1 indices))
        (elt cells (assoc-rh 'v2 indices))
        (elt cells (assoc-rh 'v3 indices))))

(defun sum (v rules)
  (funcall (cdr (assoc-rh (car rules) *rules*
                          :test #'equal))
           v (cdr rules)))

(defun mv-product (neighbourhoods current-state rules)
  (loop for cell-nb in neighbourhoods
        for i from 0
        collect (sum (& cell-nb current-state) rules)))

(defun fix-order (cells)
  (append (list (elt cells 2))
          (subseq cells (ceiling (length cells) 2))
          (reverse (subseq cells 0 (floor (/ (length cells) 2))))))

(defun mv-product-neumann (neighbourhoods current-state rules)
  (loop for cell-nb in neighbourhoods
        for i from 0 below (length current-state)
        collect (sum (neumann-order current-state cell-nb)
                     rules)))
