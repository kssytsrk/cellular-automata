;;;; -*-lisp-*-
;;;;
;;;; ui.lisp

(in-package #:ca)

(defparameter *cached-values* (make-hash-table :test #'equal))

(defparameter *sy* nil)
(defparameter *ey* nil)

(defun check-and-fix-bounds (&key x y)
  (and x
       (cond ((= x *window-width*) (setf x 0))
             ((< x 0) (setf x (1- *window-width*)))))
  (and y
       (cond ((= y *window-height*) (setf y 0))
             ((< y 0) (setf y (1- *window-height*)))))
  (if (and x y) (values x y) (or x y)))

(defun pixel-value (x y)
  (multiple-value-bind (x y) (check-and-fix-bounds :x x :y y)
    (or (gethash (list x y) *cached-values*)
        (setf (gethash (list x y) *cached-values*)
              (let ((surface-fp (sdl:fp sdl:*default-display*)))
                (sdl:with-pixel (pix surface-fp)
                  (car (rassoc (unpack-color (sdl:read-pixel pix x y))
                               *colors* :test #'sdl:color=))))))))

(defun 1d-neighbours-values (x y)
  (list (pixel-value (1- x) (1- y))
        (pixel-value x (1- y))
        (pixel-value (1+ x) (1- y))))

(defun neumann-neighbours-values (x y)
  (list (pixel-value     x     y)  ; v
        (pixel-value (1+ x)    y)  ; v0
        (pixel-value     x (1+ y)) ; v1
        (pixel-value (1- x) y)     ; v2
        (pixel-value     x (1- y)) ; v3
        ))

(defun moore-neighbours-values (x y)
  (list (pixel-value     x       y) ; v
        (pixel-value (1+ x)      y) ; v0
        (pixel-value     x  (1+ y)) ; v1
        (pixel-value (1- x)      y) ; v2
        (pixel-value     x  (1- y)) ; v3
        (pixel-value (1+ x) (1+ y)) ; v4
        (pixel-value (1- x) (1+ y)) ; v5
        (pixel-value (1- x) (1- y)) ; v6
        (pixel-value (1+ x) (1- y)) ; v7
        ))

(defun color-for-pixel (x y)
  (let* ((neighbours (case *neighbourhood*
                      (:1d      (1d-neighbours-values x y))
                      (:neumann (neumann-neighbours-values x y))
                      (:moore   (moore-neighbours-values x y))))
         (new-value (transition-rule neighbours)))
    (setf (gethash (list x y) *cached-values*) new-value)
    (when (not (eql new-value 0))
      (let ((1-y (check-and-fix-bounds :y (1- y)))
            (1+y (check-and-fix-bounds :y (1+ y))))
        (if *sy*
            (setf *sy* (min *sy* 1-y 1+y))
            (setf *sy* (min 1-y 1+y)))
        (if *ey*
            (setf *ey* (max *ey* 1-y 1+y))
            (setf *ey* (max 1-y 1+y)))))
    (unless (numberp new-value)
      (format t "~&Pixel at x ~a, y ~a, current value: ~a new value: ~a"
              x y
              (pixel-value x y)
              new-value)
      (format t "~&Neighbourhood: ~a" *neighbourhood*)
      (format t "~&Ruleset: ~a" *ruleset*))
    (color new-value)))

(defun evolve ()
  (let ((surface-fp (sdl:fp sdl:*default-display*))
        (sy (or *sy* (if (eql *neighbourhood* :1d) 1 0)))
        (ey (or *ey* (1- *window-height*))))
    (setf *cached-values* (clrhash *cached-values*))
    (sdl:with-pixel (pix surface-fp)
      (sdl:with-color (col (sdl:color))
        (loop for y from sy to ey
              do (loop for x from 0 below *window-width*
                       do (sdl:write-pixel
                           pix x y
                           (apply #'sdl-cffi::sdl-map-rgba
                                  (concatenate 'list
                                               (list (sdl-base:pixel-format surface-fp))
                                               (sdl:fp (color-for-pixel x y)))))))))))

(defun initialize (&optional shapes)
  (sdl:with-init (sdl:sdl-init-video)
    (sdl:window *window-width* *window-height*
                :title-caption "Cellular automata generation")
    (setf (sdl:frame-rate) 60)
    (sdl:clear-display (color 0))

    (setf *sy* nil
          *ey* nil)

    (cond (shapes
           (eval shapes))
          ((eql *neighbourhood* :1d)
           (sdl:draw-pixel (sdl:point :x (/ *window-width* 2) :y 0)
                           :color (cdr (assoc 1 *colors*))))
          (t (sdl:draw-pixel (sdl:point :x (/ *window-width* 2)
                                        :y (/ *window-height* 2))
                             :color (color 1))))
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
             (when (sdl:mouse-left-p)
               (sdl:draw-pixel
                (sdl:point :x (sdl:mouse-x)
                           :y (sdl:mouse-y))
                :color (color 1))
               (setf *sy* (min *sy* (sdl:mouse-y)))
               (setf *ey* (max *ey* (sdl:mouse-y))))
             (evolve)
             (sdl:update-display)))))
