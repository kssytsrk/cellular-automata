(in-package #:ca)

(defun unpack-color (uint-color)
  (sdl:color :r (logand (ash uint-color -16) 255)
             :g (logand (ash uint-color -8) 255)
             :b (logand uint-color 255)))

(defun *cached-values* nil)

(defun get-pixel-value (x y)
  (if (= x *window-width*)
        (setf x 0))
  (if (= y *window-height*)
      (setf y 0))
  (if (< x 0)
      (setf x (1- *window-width*)))
  (if (< y 0)
      (setf y (1- *window-height*)))
  (let ((surface-fp (sdl:fp sdl:*default-display*)))
    (sdl:with-pixel (pix surface-fp)
      (car (rassoc (unpack-color (sdl:read-pixel pix x y))
                   *colors* :test #'sdl:color=)))))

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
  (let ((surface-fp (sdl:fp sdl:*default-display*))
        (sx 0)
        (sy (if (eql *neighbourhood* :1d) 1 0)))
    (declare (type fixnum *window-height* *window-width* sx sy)
             (optimize (safety 3) (speed 3)))
    (sdl:with-pixel (pix surface-fp)
      (sdl:with-color (col (sdl:color))
        (loop for y from sy below *window-height*
              do (loop for x from sx below *window-width*
                       do (sdl:write-pixel
                           pix x y
                           (apply #'sdl-cffi::sdl-map-rgba
                                  (concatenate 'list
                                               (list (sdl-base:pixel-format surface-fp))
                                               (sdl:fp (get-color-for-pixel x y)))))))))))

(defun initialize (&optional p1 p2)
  (sdl:with-init (sdl:sdl-init-video)
    (sdl:window *window-width* *window-height*
                :title-caption "Cellular automata generation"
                :async-blit t)
    (setf (sdl:frame-rate) 60)
    (sdl:clear-display (get-color 0))

    (cond ((and p1 p2)
           (sdl:draw-line p1 p2
                          :color (get-color 1)))
          ((eql *neighbourhood* :1d)
           (sdl:draw-pixel (sdl:point :x (/ *window-width* 2) :y 0)
                           :color (cdr (assoc 1 *colors*))))
          (t (sdl:draw-pixel (sdl:point :x (/ *window-width* 2)
                                       :y (/ *window-height* 2))
                            :color (get-color 1))))
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
