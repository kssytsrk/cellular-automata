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
             ;; Change the color of the box if the left mouse button is depressed
             ;; (when (sdl:mouse-left-p)
             ;;   (setf *random-color* (sdl:color :r (random 255) :g (random 255) :b (random 255))))
             ;; Clear the display each game loop
             ;; Draw the box having a center at the mouse x/y coordinates.
             ;; (sdl:draw-box (sdl:rectangle-from-midpoint-* (sdl:mouse-x) (sdl:mouse-y) 20 20)
             ;;               :color *random-color*)
             ;; Redraw the display
             (sdl:update-display)))))
