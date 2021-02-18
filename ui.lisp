(in-package #:ca)

(defun get-pixel-value (x y)
  (if (= x *window-width*)
      (setf x 0))
  (if (= y *window-height*)
      (setf y 0))
  (if (< x 0)
      ;(return-from get-pixel-value 0)
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

(defun get-color-for-pixel (x y d)
  (assoc-rh
   (if (eql *ruleset* :game-of-life)
       (get-game-of-life-transition-rule
        (get-moore-adj-pixel-values x y))
       (get-transition-rule
        (case d
          (1        (get-1d-adj-pixel-values x y))
          (:neumann (get-neumann-adj-pixel-values x y))
          (:moore   (get-moore-adj-pixel-values x y)))))
   *colors*))

(defun redraw-ca (d &optional start)
  (unless start
    (if (eql d 1)
        (setf start (sdl:point :x 0 :y 1))
        (setf start (sdl:point))))
  (loop for y from (sdl:y start) below *window-height*
        do (loop for x from (sdl:x start) below *window-width*
                 do (sdl:draw-pixel (sdl:point :x x :y y)
                                    :color (get-color-for-pixel x y d)))
             (sdl:update-display)))

(defun initialize (d &optional p1 p2)
  (sdl:with-init ()
    (sdl:window *window-width* *window-height*
                :title-caption "Cellular automata generation")
    (setf (sdl:frame-rate) 60)

    (sdl:clear-display (cdr (assoc 0 *colors*)))

    (if (eql d 1)
        (sdl:draw-pixel (sdl:point :x (/ *window-width* 2) :y 0)
                        :color (cdr (assoc 1 *colors*)))
        (sdl:draw-pixel (sdl:point :x (/ *window-width* 2)
                                   :y (/ *window-height* 2))
                        :color (cdr (assoc 1 *colors*))))

    (if (and (eql *ruleset* :game-of-life)
             p1 p2)
        (sdl:draw-line p1 p2
                       :color (cdr (assoc 1 *colors*))))
    (redraw-ca d)

    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
             (if (sdl:mouse-left-p)
                 (progn (sdl:draw-pixel
                         (sdl:point :x (sdl:mouse-x)
                                    :y (sdl:mouse-y))
                         :color (cdr (assoc 1 *colors*)))
                        (redraw-ca d))
                 (redraw-ca d))))))
