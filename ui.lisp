(in-package #:ca)

(require :sdl2)

(defparameter *screen-width* 200)
(defparameter *screen-height* 200)

(defparameter *world-matrix* nil
  "The environment of a cellular automaton.")

(defparameter *bg-color* '(0 0 0 255))
(defparameter *fg-color* '(255 255 255 255))

(defun render-all-points (renderer world)
  (loop for i from 0 to (1- *screen-height*)
        do (loop for j from 0 to (1- *screen-width*)
                 when (eql (world-aref world i j) 1)
                   do (apply #'sdl2:set-render-draw-color renderer *fg-color*)
                 else
                   do (apply #'sdl2:set-render-draw-color renderer *bg-color*)
                 end
                 do (sdl2::render-draw-point renderer i j))))

(defun initialize ()
  "Start program."
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "SDL2 Renderer API Demo"
                           :flags '(:shown)
                           :w *screen-width*
                           :h *screen-height*)
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keyup
           (:keysym keysym)
           (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
             (sdl2:push-event :quit)))
          (:idle
           ()
           (setf *world-matrix* (make-world :rows *screen-height*
                                            :cols *screen-width*
                                            :states '(0 1)))
           (setf (world-aref *world-matrix* 0 (/ (world-cols *world-matrix*) 2)) 1)
           (calculate-world *world-matrix*)
           (render-all-points renderer *world-matrix*)
           (sdl2:render-present renderer)
	       (sdl2:delay 33))
          (:quit () t))))))
