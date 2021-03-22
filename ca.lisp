;;;; -*-lisp-*-
;;;;
;;;; ca.lisp

(in-package #:ca)

(defun 1d-neighbourhood (n &optional (nc 3))
  (lambda (cell)
    (let ((start (- cell (/ (- nc 1) 2)))
          (end   (+ cell (/ (- nc 1) 2)))
          (list  (make-list n :initial-element 0))
          out-of-bounds)
      (when (< start 0)
        (fill list 1 :start (+ n start))
        (setf out-of-bounds (cons 'start (+ n start)))
        (setf start 0))
      (when (> end (1- n))
        (fill list 1 :end (- end (1- n)))
        (setf out-of-bounds (cons 'end (- end (1- n))))
        (setf end (1- n)))
      (cons (fill list 1 :start start :end (1+ end)) out-of-bounds))))

(defun 1d-neighbourhoods (n &optional (nc 3))
  (mapcar (1d-neighbourhood n nc) (loop for i from 0 below n collect i)))

(defun & (bm v2)
  (remove-if #'null
             (fix-bounds
              (cdr bm)
     (mapcar (lambda (a1 a2) (and (eql 1 a1) a2))
             (car bm) v2))))

(defun fix-bounds (out-of-bounds list)
  (case (car out-of-bounds)
    (start (append (subseq list (cdr out-of-bounds))
                   (subseq list 0 (cdr out-of-bounds))))
    (end   (append (subseq list (cdr out-of-bounds))
                   (subseq list 0 (cdr out-of-bounds))))
    (t     list)))

(defun sum (v rules)
  (funcall (cdr (assoc-rh (car rules) *rules*
                          :test #'equal))
           v (cdr rules)))

(defun mv-product (neighbourhoods current-state rules)
  (loop for cell-nb in neighbourhoods
        for i from 0
        collect (sum (& cell-nb current-state) rules)))
