(in-package #:ca)

(defvar *cached-values* (make-hash-table :test #'equal))

(defun pixel-value (x y)
  (if (= x *window-width*)
      (setf x 0))
  (if (= y *window-height*)
      (setf y 0))
  (if (< x 0)
      (setf x (1- *window-width*)))
  (if (< y 0)
      (setf y (1- *window-height*)))
  (or (gethash (list x y) *cached-values*)
      (setf (gethash (list x y) *cached-values*)
            (let ((surface-fp (sdl:fp sdl:*default-display*)))
              (sdl:with-pixel (pix surface-fp)
                (car (rassoc (unpack-color (sdl:read-pixel pix x y))
                             *colors* :test #'sdl:color=)))))))

(defun 1d-adj-pixel-values (x y)
  (list (pixel-value (1- x) (1- y))
        (pixel-value x (1- y))
        (pixel-value (1+ x) (1- y))))

(defun neumann-adj-pixel-values (x y)
  (list (pixel-value     x     y)  ; v
        (pixel-value (1+ x)    y)  ; v0
        (pixel-value     x (1+ y)) ; v1
        (pixel-value (1- x) y)     ; v2
        (pixel-value     x (1- y)) ; v3
        ))

(defun moore-adj-pixel-values (x y)
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
  (let ((new-value (transition-rule
                    (case *neighbourhood*
                      (:1d      (1d-adj-pixel-values x y))
                      (:neumann (neumann-adj-pixel-values x y))
                      (:moore   (moore-adj-pixel-values x y))))))
    (setf (gethash (list x y) *cached-values*) new-value)
    (color new-value)))

(defun redraw-ca ()
  (let ((surface-fp (sdl:fp sdl:*default-display*))
        (sx 0)
        (sy (if (eql *neighbourhood* :1d) 1 0)))
    (sdl:with-pixel (pix surface-fp)
      (sdl:with-color (col (sdl:color))
        (loop for y from sy below *window-height*
              do (loop for x from sx below *window-width*
                       do (sdl:write-pixel
                           pix x y
                           (apply #'sdl-cffi::sdl-map-rgba
                                  (concatenate 'list
                                               (list (sdl-base:pixel-format surface-fp))
                                               (sdl:fp (color-for-pixel x y)))))))))
    (setf *cached-values* (clrhash *cached-values*))))

(defun initialize (&optional p1 p2)
  (sdl:with-init (sdl:sdl-init-video)
    (sdl:window *window-width* *window-height*
                :title-caption "Cellular automata generation"
                :async-blit t)
    (setf (sdl:frame-rate) 60)
    (sdl:clear-display (color 0))

    (cond ((and p1 p2)
           (sdl:draw-line p1 p2
                          :color (color 1)))
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
                :color (color 1)))
             (redraw-ca)
             (sdl:update-display)))))
