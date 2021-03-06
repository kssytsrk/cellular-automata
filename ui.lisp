;;;; -*-lisp-*-
;;;;
;;;; ui.lisp

(in-package #:ca)

(defun draw-text (x y color &key neighbourhood ruleset cur-steps bg-color)
  "Draws the info text."
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

(defun draw-state (state colors sy sx ey ex)
  "Draws the state STATE using color set COLORS starting from sy and sx,
and ending at ey and ex."
  (let ((surface-fp (sdl:fp sdl:*default-display*)))
    (sdl:with-pixel (pix surface-fp)
      (loop for y from sy below ey
            do (loop for x from sx below ex
                     do (write-pixel pix x y
                                     (color (elt state (+ x (* (- y sy) ex)))
                                            colors)))))))

(defun start (&key (h 300) (w 600)
                   (dimensions 1)
                (ruleset 1) (neighbourhood :elementary) (tag nil)
                (steps nil) (colors :grayscale) (states 2) (auto t)
                (padding 30) (starting-state '(:center-dot)))
  "Starts the program."
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
        (state (funcall (apply (eval `(state-fn ,(car starting-state)))
                               (cdr starting-state))
                        (case dimensions (1 1) (t h)) w)))
    (sdl:with-init ()
      (sdl:window w (+ h padding)
                  :title-caption "Cellular automata generation"
                  :no-frame t)
      (setf (sdl:frame-rate) 60)
      (sdl:clear-display (color 0 colors))

      (sdl:initialise-default-font)
      (draw-text 1 (+ h 1)
                 (color (1- states) colors)
                 :neighbourhood neighbourhood
                 :ruleset ruleset
                 :cur-steps cur-steps
                 :bg-color (color 0 colors))
      (sdl:update-display)

      (sdl:with-events ()
        (:quit-event () t)
        (:key-up-event (:key key)
                       (cond ((sdl:key= key :SDL-KEY-ESCAPE)
                              (sdl:push-quit-event))
                             ((sdl:key= key :SDL-KEY-SPACE)
                              (case dimensions
                                (1 (draw-state state colors cur-steps 0 (1+ cur-steps) w))
                                (2 (draw-state state colors 0 0 h w)))
                              (setf state (next-state (cdr neighbourhood) state ruleset))
                              (draw-text 1 (+ h 1)(color (1- states) colors)
                                         :cur-steps cur-steps
                                         :bg-color (color 0 colors))
                              (incf cur-steps)
                              (sdl:update-display))))
        (:idle ()
               (when (sdl:mouse-left-p)
                 (sdl:draw-pixel
                  (sdl:point :x (sdl:mouse-x)
                             :y (sdl:mouse-y))
                  :color (color 1 colors)))
               (unless (or (not auto) (and steps (< steps cur-steps)))
                 (case dimensions
                   (1 (draw-state state colors cur-steps 0 (1+ cur-steps) w))
                   (2 (draw-state state colors 0 0 h w)))
                 (setf state (next-state (cdr neighbourhood) state ruleset))
                 (draw-text 1 (+ h 1)(color (1- states) colors)
                            :cur-steps cur-steps
                            :bg-color (color 0 colors))
                 (incf cur-steps)
                 (sdl:update-display)))))))
