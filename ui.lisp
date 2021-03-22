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

(defun draw-next-generation (state colors sy ey)
  (let ((surface-fp (sdl:fp sdl:*default-display*)))
    (sdl:with-pixel (pix surface-fp)
      (loop for y from sy to ey
            do (loop for i in state
                     for x from 0
                     do (write-pixel pix x y
                                     (color i colors)))))))

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
                             (funcall (car (assoc-rh tag *rules*
                                                     :test #'equal))
                                      ruleset neighbourhood
                                      states))))
        ((not (realp ruleset))
         (setf ruleset (list ruleset t)))
        (t
         (setf ruleset (cons :normal
                             (funcall (car (assoc-rh :normal *rules*
                                                     :test #'equal))
                                      ruleset neighbourhood
                                      states)))))
  (unless (cdr ruleset)
    (error "Invalid ruleset input."))

  (let ((cur-steps 0)
        (state (make-list w :initial-element 0)))
    (sdl:with-init ()
      (sdl:window w (+ h *padding*)
                  :title-caption "Cellular automata generation"
                  :no-frame t)
      (setf (sdl:frame-rate) 60)
      (sdl:clear-display (color 0 colors))

      (setf (elt state (truncate (/ w 2))) 1)

      (sdl:initialise-default-font)
      (draw-text 1 (+ h 1)
                 (color (1- states) colors)
                 :neighbourhood neighbourhood
                 :ruleset ruleset
                 :cur-steps cur-steps
                 :bg-color (color 0 colors))

      (let ((neighbourhoods (1d-neighbourhoods w))
            pause)
        (sdl:with-events ()
          (:quit-event () t)
          (:key-up-event (:key key)
                         (cond ((sdl:key= key :SDL-KEY-ESCAPE)
                                (sdl:push-quit-event))
                               ((sdl:key= key :SDL-KEY-SPACE)
                                (if auto
                                    (setf pause (not pause))
                                    (progn
                                      (draw-next-generation state colors cur-steps cur-steps)
                                      (setf state (mv-product neighbourhoods state ruleset))
                                      (incf cur-steps)))
                                (sdl:update-display))))
          (:idle ()
                 (when (sdl:mouse-left-p)
                   (sdl:draw-pixel
                    (sdl:point :x (sdl:mouse-x)
                               :y (sdl:mouse-y))
                    :color (color 1 colors)))
                 (unless (or pause (not auto))
                   (draw-next-generation state colors cur-steps cur-steps)
                   (setf state (mv-product neighbourhoods state ruleset))
                   (incf cur-steps)
                   (draw-text 1 (+ h 1)
                              (color (1- states) colors)
                              :cur-steps cur-steps
                              :bg-color (color 0 colors))
                   (if (and steps (< steps cur-steps))
                       (setf pause t))
                   (sdl:update-display))))))))

;; (defun start (&key (h 300) (w 600)
;;                 (ruleset 1) (neighbourhood :elementary) (tag nil)
;;                 (steps nil)
;;                 (colors :grayscale) (states 2) (auto t)
;;                 starting-pixels)
;;   (when (and steps
;;              (eql neighbourhood :elementary))
;;     (setf h steps)
;;     (setf w (* 2 h)))
;;   (case colors
;;     (:golly     (setf colors *colors-golly*))
;;     (:grayscale
;;      (setf colors *colors-grayscale*)
;;      (setf colors (acons (1- states)
;;                          (sdl:color :r 0 :g 0 :b 0 :a 255)
;;                          colors))))

;;   (cond ((not (null tag))
;;          (setf ruleset (cons tag
;;                              (funcall (car (assoc-rh tag *rules*
;;                                                      :test #'equal))
;;                                       ruleset neighbourhood states))))
;;         ((not (realp ruleset))
;;          (setf ruleset (list ruleset t)))
;;         (t (setf ruleset (cons :normal (funcall (car (assoc-rh :normal *rules*
;;                                                                :test #'equal))
;;                                                 ruleset neighbourhood states)))))
;;   (unless (cdr ruleset)
;;     (error "Invalid ruleset input."))

;;   (let ((cur-steps 0))
;;     (sdl:with-init (sdl:sdl-init-video)
;;       (sdl:window w (+ h *padding*)
;;                   :title-caption "Cellular automata generation"
;;                   :no-frame t)
;;       (setf (sdl:frame-rate) 60)
;;       (sdl:clear-display (color 0 colors))
;;       (sdl:initialise-default-font)
;;       (sdl:draw-string-solid (format nil
;;                                      "Neighbourhood: ~a"
;;                                      (string-downcase
;;                                       (symbol-name neighbourhood)))
;;                              (sdl:point :x 1 :y (+ h 1))
;;                              :color (color (1- states) colors))
;;       (sdl:draw-string-solid (format nil
;;                                      "Ruleset: ~a"
;;                                      (string-downcase
;;                                       (symbol-name (car ruleset))))
;;                              (sdl:point :x 1 :y (+ h 9))
;;                              :color (color (1- states) colors))
;;       (sdl:draw-string-shaded-* (format nil
;;                                         "Steps: ~a"
;;                                         (write-to-string cur-steps))
;;                                 1 (+ h 19)
;;                                 (color (1- states) colors)
;;                                 (color 0 colors))



;;       (setf *sy* nil
;;             *ey* nil)

;;       (if starting-pixels
;;           (eval starting-pixels)
;;           (draw-starting-pixels neighbourhood (car ruleset) colors))

;;       (let ((evolve-fn (if (eql tag :neighbour-number)
;;                            #'evolve-from-prevgen
;;                            #'evolve))
;;             pause)
;;         (sdl:with-events ()
;;           (:quit-event () t)
;;           (:key-up-event (:key key)
;;                          (cond ((sdl:key= key :SDL-KEY-ESCAPE)
;;                                 (sdl:push-quit-event))
;;                                ((sdl:key= key :SDL-KEY-SPACE)
;;                                 (if auto
;;                                     (setf pause (not pause))
;;                                     (progn
;;                                       (funcall evolve-fn
;;                                                neighbourhood ruleset colors)
;;                                       (incf steps)))
;;                                 (sdl:update-display))))
;;           (:idle ()
;;                  (when (sdl:mouse-left-p)
;;                    (sdl:draw-pixel
;;                     (sdl:point :x (sdl:mouse-x)
;;                                :y (sdl:mouse-y))
;;                     :color (color 1 colors))
;;                    (setf *sy* (min *sy* (sdl:mouse-y)))
;;                    (setf *ey* (max *ey* (sdl:mouse-y))))
;;                  (unless (or pause (not auto))
;;                    (funcall evolve-fn neighbourhood ruleset colors)
;;                    (incf cur-steps)
;;                    (if (or (and steps (< steps cur-steps))
;;                            (and (< 1 cur-steps)
;;                                 (eq neighbourhood :elementary)))
;;                        (setf pause t)
;;                        (sdl:draw-string-shaded-* (format nil
;;                                                          "Steps: ~a"
;;                                                          (if (eql neighbourhood
;;                                                                   :elementary)
;;                                                              h
;;                                                              (write-to-string cur-steps)))
;;                                                  1 (+ h 19)
;;                                                  (color (1- states) colors)
;;                                                  (color 0 colors)))
;;                    (sdl:update-display))))))))
