(in-package #:ca)

(defun redraw-ca ()
  (loop for i below *window-height*
        do (loop for j below *window-width*
                 do (sdl:draw-pixel (sdl:point :x j :y i)
                                    :color (cdr (assoc (world-aref *world* i j)
                                                       *colors*))))))

(defun initialize ()
  (sdl:with-init ()
    (sdl:window *window-width* *window-height*
                :title-caption "Cellular automata generation")
    (setf (sdl:frame-rate) 60)

    (sdl:clear-display (cdr (assoc 0 *colors*)))
    (redraw-ca)

    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
                       (sdl:push-quit-event))
      (:idle ()
             (when (sdl:mouse-left-p)
               (setf (world-aref *world* (sdl:mouse-y) (sdl:mouse-x)) 1)
               (calculate-world *world* *ruleset* (sdl:mouse-y))
               (redraw-ca))

             ;; Redraw the display
             (sdl:update-display)))))
