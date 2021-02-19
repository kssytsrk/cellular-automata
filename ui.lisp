(in-package #:ca)

(defun get-pixel-value (x y)
  (if (= x *window-width*)
      (setf x 0))
  (if (= y *window-height*)
      (setf y 0))
  (if (< x 0)
      (setf x (1- *window-width*)))
  (if (< y 0)
      (setf y (1- *window-height*)))
  (car (rassoc (sdl:read-pixel (sdl:point :x x :y y)
                               :surface sdl:*default-display*)
               *colors*
               :test #'sdl:color=)))

(defun get-1d-adj-pixel-values (x y)
  (list (get-pixel-value (1- x) (1- y))
        (get-pixel-value x (1- y))
        (get-pixel-value (1+ x) (1- y))))

(defun get-neumann-adj-pixel-values (x y)
  (list (get-pixel-value     x     y)  ; v
        (get-pixel-value (1+ x)    y)  ; v0
        (get-pixel-value     x (1+ y)) ; v1
        (get-pixel-value (1- x) y)     ; v2
        (get-pixel-value     x (1- y)) ; v3
        ))

(defun get-moore-adj-pixel-values (x y)
  (list (get-pixel-value     x       y) ; v
        (get-pixel-value (1+ x)      y) ; v0
        (get-pixel-value     x  (1+ y)) ; v1
        (get-pixel-value (1- x)      y) ; v2
        (get-pixel-value     x  (1- y)) ; v3
        (get-pixel-value (1+ x) (1+ y)) ; v4
        (get-pixel-value (1- x) (1+ y)) ; v5
        (get-pixel-value (1- x) (1- y)) ; v6
        (get-pixel-value (1+ x) (1- y)) ; v7
        ))

(defun get-color-for-pixel (x y)
  (get-color
   (get-transition-rule
    (case *neighbourhood*
      (:1d      (get-1d-adj-pixel-values x y))
      (:neumann (get-neumann-adj-pixel-values x y))
      (:moore   (get-moore-adj-pixel-values x y))))))

(defun redraw-ca ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (declare (type fixnum *window-height* *window-width*))
  (loop for y from (if (eql *neighbourhood* :1d) 1 0) below *window-height*
        do (loop for x from 0 below *window-width*
                 do (sdl:draw-pixel (sdl:point :x x :y y)
                                    :color (get-color-for-pixel x y)))))

(defun initialize (&optional p1 p2)
  (sdl:with-init ()
    (sdl:init-video)
    (sdl:window *window-width* *window-height*
                :title-caption "Cellular automata generation"
                :video-driver (sdl:video-driver-name))
    (setf (sdl:frame-rate) 60)

    (sdl:clear-display (get-color 0))

    (if (eql *neighbourhood* :1d)
        (sdl:draw-pixel (sdl:point :x (/ *window-width* 2) :y 0)
                        :color (cdr (assoc 1 *colors*)))
        (sdl:draw-pixel (sdl:point :x (/ *window-width* 2)
                                   :y (/ *window-height* 2))
                        :color (get-color 1)))

    (if (and p1 p2)
        (sdl:draw-line p1 p2
                       :color (get-color 1)))
    (redraw-ca)

    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
             (when (sdl:mouse-left-p)
               (sdl:draw-pixel
                (sdl:point :x (sdl:mouse-x)
                           :y (sdl:mouse-y))
                :color (get-color 1)))
             (redraw-ca)
             (sdl:update-display)))))
