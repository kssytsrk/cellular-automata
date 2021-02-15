(in-package #:ca)

(defun get-pixel-value (x y)
  (if (= x *window-width*)
      (setf x 0))
  (if (= y *window-height*)
      (setf y 0))
  (if (< x 0)
      ;(return-from get-pixel-value 0)
      (setf x (1- *window-width*))
      )
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

(defun get-2d-adj-pixel-values (x y)
  (list (get-pixel-value     x     y)  ; v
        (get-pixel-value (1+ x)    y)  ; v0
        (get-pixel-value     x (1+ y)) ; v1
        (get-pixel-value (1- x) y)     ; v2
        (get-pixel-value     x (1- y)) ; v3
))

(defun get-color-for-pixel (x y d)
  (assoc-rh
   (assoc-rh (case d
               (1 (get-1d-adj-pixel-values x y))
               (2 (get-2d-adj-pixel-values x y)))
             *ruleset*
             :test #'equal)
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


;; Now for the 2-dimensional case you have to specify which neighborhood your rule is on. Typical choices are the center cell plus its 4 nearest neighbors which gives a 5-cell neighborhood in form of a cross. This is known as the v.Neumann neighborhood. There the possible rules are numbered from 0 to 2^32. The other typical choice is a neighborhood of 9 cells (the center plus its eight surrounding cells forming a square). This is known as Moore neighborhood. For this setting you get 2^(2^9)=2^512 rules. Not sure which case the rule you are quoting is from.
;; make a fn that generates rules! also, write it in the rules.lisp file

(defun initialize (d)
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
    (redraw-ca d)

    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
             (when (sdl:mouse-left-p)
               (sdl:draw-pixel (sdl:point :x (sdl:mouse-x) :y (sdl:mouse-y)) :color (cdr (assoc 1 *colors*)))
               (redraw-ca d))))))
