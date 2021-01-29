(in-package #:ca)

(require :sdl2)

(defun render-all-points (renderer world)
  "Re-draw all points according to the WORLD's matrix."
  (loop for i from 0 to (1- *screen-height*)
        do (loop for j from 0 to (1- *screen-width*)
                 when (eql (world-aref world i j) 1)
                   do (apply #'sdl2:set-render-draw-color renderer *fg-color*)
                 else
                   do (apply #'sdl2:set-render-draw-color renderer *bg-color*)
                 end
                 do (sdl2::render-draw-point renderer j i))))

(defun initialize (screen-height screen-width world)
  "Start GUI."
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "SDL2 Renderer API Demo"
                           :flags '(:shown)
                           :w screen-width
                           :h screen-height)
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keyup
           (:keysym keysym)
           (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
             (sdl2:push-event :quit)))
          (:idle
           ()
           (render-all-points renderer world)
           (sdl2:render-present renderer)
           (sdl2:delay 33))
          (:quit () t))))))
