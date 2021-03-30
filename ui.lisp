;;;; -*-lisp-*-
;;;;
;;;; ui.lisp

(in-package #:ca)

(defparameter *padding* 30)

;;; only valid within with-pixel/s
(defmacro write-pixel (pix x y color)
  `(sdl:write-pixel
    ,pix ,x ,y
    (apply #'sdl-cffi::sdl-map-rgba
	   (concatenate 'list
			(list (sdl-base:pixel-format surface-fp))
			(sdl:fp ,color)))))

(defun draw-text (x y color &key neighbourhood ruleset cur-steps bg-color)
  (if neighbourhood
      (sdl:draw-string-solid (format nil
                                   "Neighbourhood: ~a"
                                   (string-downcase
                                    (symbol-name neighbourhood)))
                           (sdl:point :x x :y y)
                           :color color))
  (if ruleset
      (sdl:draw-string-solid (format nil
                                   "Ruleset: ~a"
                                   (string-downcase
                                    (symbol-name (car ruleset))))
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
      (loop for y from sy to ey
            do (loop for x from 0 below ex
                     ;do (princ (elt state (+ x (* (- y sy) ex))))
                     do (write-pixel pix x y
                                     (color (elt state (+ x (* (- y sy) ex)))
                                            colors)))))))

(defun start (&key (h 300) (w 600)
                (ruleset 1) (neighbourhood :elementary) (tag nil)
                (steps nil)
                (colors :grayscale) (states 2) (auto t))
  (when (and steps
             (eql neighbourhood :elementary))
    (setf h steps)
    (setf w (* 2 h)))
  (if (and (eql neighbourhood :elementary)
           (not steps))
      (setf steps h))

  (case colors
    (:golly     (setf colors *colors-golly*))
    (:grayscale (setf colors *colors-grayscale*)
     (setf colors (acons (1- states)
                         (sdl:color :r 0 :g 0 :b 0 :a 255)
                         colors))))
  (cond ((not (null tag))
         (setf ruleset (cons tag
                             (funcall (car (assoc-rh tag *rules* :test #'equal))
                                      ruleset neighbourhood states))))
        ((not (realp ruleset))
         (setf ruleset (list ruleset t)))
        (t
         (setf ruleset (cons :normal
                             (funcall (car (assoc-rh :normal *rules* :test #'equal))
                                      ruleset neighbourhood states)))))
  (unless (cdr ruleset)
    (error "Invalid ruleset input."))

  (let ((cur-steps 0)
        (state (if (eql neighbourhood
                        :elementary)
                   (make-array w :initial-element 0)
                   (make-array (* w h) :initial-element 0))))
        (sdl:with-init ()
      (sdl:window w (+ h *padding*)
                  :title-caption "Cellular automata generation"
                  :no-frame t)
      (setf (sdl:frame-rate) 60)
      (sdl:clear-display (color 0 colors))

      (if (eql neighbourhood :elementary)
          (setf (elt state (truncate (/ w 2))) 1)
          (setf (elt state (truncate (+ (/ w 2) (* w (/ h 2))))) 1))

      (sdl:initialise-default-font)
      (draw-text 1 (+ h 1)
                 (color (1- states) colors)
                 :neighbourhood neighbourhood
                 :ruleset ruleset
                 :cur-steps cur-steps
                 :bg-color (color 0 colors))

      (let ((neighbourhoods (if (eql neighbourhood :elementary)
                                (1d-neighbourhoods w)
                                (neumann-neighbourhoods h w)))
            pause)
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
                 (unless (or pause (not auto))
                   (if (eql neighbourhood :elementary)
                       (draw-next-generation state colors cur-steps cur-steps
                                             w)
                       (draw-next-generation state colors 0 (1- h) w))
                   (setf state (if (eql neighbourhood :elementary)
                                   (mv-product neighbourhoods
                                               state ruleset)
                                   (mv-product-neumann
                                    neighbourhoods
                                    state
                                    ruleset)))
                   ;; (print-ca state w)
                   (incf cur-steps)
                   (draw-text 1 (+ h 1)
                              (color (1- states) colors)
                              :cur-steps cur-steps
                              :bg-color (color 0 colors))
                   (if (and steps (< steps cur-steps))
                       (setf pause t))
                   (sdl:update-display))))))))

(defun start-2 ()
  (sdl:with-init ()
    (sdl:window 50 (+ 50 *padding*)
                :title-caption "Cellular automata generation"
                :no-frame t)
    (setf (sdl:frame-rate) 60)
    (sdl:clear-display (color 0 *colors-grayscale*))

    (print-ca (fill (make-list (* 50 50) :initial-element 0) 1 :start 52 :end 63) 50)
    (draw-next-generation
     (fill (make-list (* 50 50) :initial-element 0) 1 :start 52 :end 63)
     *colors-grayscale* 0 49 50)
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()))))
