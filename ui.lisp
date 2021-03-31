;;;; -*-lisp-*-
;;;;
;;;; ui.lisp

(in-package #:ca)

(defun draw-text (x y color &key neighbourhood ruleset cur-steps bg-color)
  (if neighbourhood
      (sdl:draw-string-solid (format nil
                                   "Neighbourhood: ~a"
                                   (string-downcase
                                    (symbol-name (car neighbourhood))))
                           (sdl:point :x x :y y)
                           :color color))
  (if ruleset
      (sdl:draw-string-solid (format nil
                                   "Ruleset: ~a ~@[#~a~]"
                                   (string-downcase
                                    (symbol-name (car (car ruleset))))
                                   (cdr (car ruleset)))
                           (sdl:point :x x :y (+ y 9))
                           :color color))
  (if cur-steps
      (sdl:draw-string-shaded-* (format nil
                                      "Steps: ~a"
                                      (write-to-string cur-steps))
                              x (+ y 19)
                              color
                              bg-color)))

(defun draw-next-generation (state colors sy ey ex)
  (let ((surface-fp (sdl:fp sdl:*default-display*)))
    (sdl:with-pixel (pix surface-fp)
      (loop for y from sy below ey
            do (loop for x from 0 below ex
                     do (write-pixel pix x y
                                     (color (elt state (+ x (* (- y sy) ex)))
                                            colors)))))))

(defun start (&key (h 300) (w 600)
                   (dimensions 1)
                (ruleset 1) (neighbourhood :elementary) (tag nil)
                (steps nil)
                (colors :grayscale) (states 2) (auto t) (padding 30))
  (when (eql dimensions 1)
    (if steps
        (progn
          (setf h steps)
          (setf w (* 2 h)))
        (setf steps h)))

  (case colors
    (:golly     (setf colors *colors-golly*))
    (:grayscale (setf colors *colors-grayscale*)
     (setf colors (acons (1- states)
                         (sdl:color :r 0 :g 0 :b 0 :a 255)
                         colors))))
  (handler-case
      (progn
        (setf ruleset
                    (cons (or (and tag (cons tag ruleset))
                        (unless (realp ruleset) (cons ruleset nil))
                        (cons :normal ruleset))
                    (funcall
                     (eval `(ruleset-fn ,(or tag (unless (realp ruleset) ruleset) :normal)))
                     ruleset neighbourhood states)))
        (unless (cdr ruleset)
          (error "The ruleset generation function returned NIL.")))
    (t (error) (error (format nil "Invalid ruleset input.~&Details: ~a"
                              error))))
  (handler-case
      (setf neighbourhood
            (cons neighbourhood
                  (cell-neighbourhoods (neighbourhood-variable neighbourhood)
                                       (case dimensions
                                         (1 (list w))
                                         (2 (list h w))))))
    (t (error) (error (format nil "Invalid neighbourhood input.~&Details: ~a"
                              error))))

  (let ((cur-steps 0)
        (state (case dimensions
                 (1 (make-array w :initial-element 0))
                 (2 (make-array (* w h) :initial-element 0)))))
    (sdl:with-init ()
      (sdl:window w (+ h padding)
                  :title-caption "Cellular automata generation"
                  :no-frame t)
      (setf (sdl:frame-rate) 60)
      (sdl:clear-display (color 0 colors))

      (case dimensions
        (1 (setf (elt state (truncate (/ w 2))) 1))
        (2 (setf (elt state (truncate (+ (/ w 2) (* w (/ h 2))))) 1)))

      (sdl:initialise-default-font)
      (draw-text 1 (+ h 1)
                 (color (1- states) colors)
                 :neighbourhood neighbourhood
                 :ruleset ruleset
                 :cur-steps cur-steps
                 :bg-color (color 0 colors))

      (sdl:with-events ()
        (:quit-event () t)
        (:key-up-event (:key key)
                       (cond ((sdl:key= key :SDL-KEY-ESCAPE)
                              (sdl:push-quit-event))))
        (:idle ()
               (when (sdl:mouse-left-p)
                 (sdl:draw-pixel
                  (sdl:point :x (sdl:mouse-x)
                             :y (sdl:mouse-y))
                  :color (color 1 colors)))
               (unless (or (not auto) (and steps (< steps cur-steps)))
                 (case dimensions
                   (1 (draw-next-generation state colors cur-steps (1+ cur-steps) w))
                   (2 (draw-next-generation state colors 0 h w)))
                 (setf state (next-state (cdr neighbourhood) state ruleset))
                 (draw-text 1 (+ h 1)(color (1- states) colors)
                            :cur-steps cur-steps
                            :bg-color (color 0 colors))
                 (incf cur-steps)
                 (sdl:update-display)))))))
