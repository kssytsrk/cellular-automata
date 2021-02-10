(in-package #:ca)

(defun get-pixel-value (x y)
(defun redraw-ca (&optional (y-start 0))
  (loop for y from y-start below (1- *window-height*)
        do (loop for x below *window-width*
                 do (sdl:draw-pixel (sdl:point :x x :y (1+ y))
                                    :color
                                    (assoc-rh
                                     (assoc-rh (list (get-pixel-value (1- x) y)
                                                     (get-pixel-value x y)
                                                     (get-pixel-value (1+ x) y))
                                               *ruleset*
                                               :test #'equal)
                                     *colors*)))
           (sdl:update-display)))

(defun initialize ()
  (sdl:with-init ()
    (sdl:window *window-width* *window-height*
                :title-caption "Cellular automata generation")
    (setf (sdl:frame-rate) 60)

    (sdl:clear-display (cdr (assoc 0 *colors*)))
    (sdl:draw-pixel (sdl:point :x (/ *window-height* 2) :y 0)
                    :color (cdr (assoc 1 *colors*)))
    (redraw-ca)

    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
             (when (sdl:mouse-left-p)
               (sdl:draw-pixel (sdl:point :x (sdl:mouse-x) :y (sdl:mouse-y))
                               :color (cdr (assoc 1 *colors*)))
               (redraw-ca (sdl:mouse-y)))))))
