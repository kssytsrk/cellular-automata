;;;; -*-lisp-*-
;;;;
;;;; ui.lisp

(in-package #:ca)

(defparameter *cached-values* (make-hash-table :test #'equal))

(defparameter *sy* nil)
(defparameter *ey* nil)

(defun evolve (neighbourhood ruleset colorset tag)
  (let ((surface-fp (sdl:fp sdl:*default-display*))
        (sy (or *sy* (if (eql neighbourhood :elementary) 1 0)))
        (ey (or *ey* (1- (elt (sdl:video-dimensions) 1)))))
    (setf *cached-values* (clrhash *cached-values*))
    (sdl:with-pixel (pix surface-fp)
      (sdl:with-color (col (sdl:color))
        (loop for y from sy to ey
              do (loop for x from 0 below (elt (sdl:video-dimensions) 0)
                       do (write-pixel pix x y (color-for-pixel ruleset
                                                                neighbourhood
                                                                colorset
                                                                tag
                                                                x y))))))))

(defun evolve-from-prevgen (neighbourhood ruleset colorset tag)
  (let ((surface-fp (sdl:fp sdl:*default-display*))
        (sy (or *sy* (if (eql neighbourhood :elementary) 1 0)))
        (ey (or *ey* (1- (elt (sdl:video-dimensions) 1)))))
    (setf *cached-values* (clrhash *cached-values*))
    (sdl:with-pixel (pix surface-fp)
      (sdl:with-color (col (sdl:color))
        (let ((new-gen
                (loop for y from sy to ey
		      with new-gen = nil
		      do (loop for x from 0 below (elt (sdl:video-dimensions) 0)
			       do (push (list x y (color-for-pixel ruleset
                                                                   neighbourhood
                                                                   colorset
                                                                   tag
                                                                   x y
                                                                   :use-cache nil))
					new-gen))
                      finally (return new-gen))))
	  (loop for element in new-gen
		do (write-pixel pix
                                (first element)
				(second element)
				(third element))
                   (setf (gethash (list (first  element)
                                        (second element))
                                  *cached-values*)
                         (rassoc (third element)
                                 *colors* :test #'sdl:color=))))))))

(defun start (&key (h 300) (w 600)
                (ruleset 1) (neighbourhood :elementary) (tag nil)
                (steps nil)
                (colors :golly)	(states 2) (auto t)
                starting-pixels)
  (when (and steps
             (eql neighbourhood :elementary))
    (setf h steps)
    (setf w (* 2 h)))
  (case colors
    (:golly     (setf colors *colors-golly*))
    (:grayscale (setf colors *colors-grayscale*)))
  (unless (or (not (realp ruleset))
              (setf ruleset (case tag
                              (:totalistic
                               (totalistic-ruleset ruleset neighbourhood states))
                              (:number-of-neighbours
                               (number-of-neighbours-ruleset ruleset neighbourhood))
                              (t (ruleset ruleset neighbourhood)))))
    (error "Invalid ruleset input."))

  (sdl:with-init (sdl:sdl-init-video)
    (sdl:window w h
                :title-caption "Cellular automata generation"
		:no-frame t)
    (setf (sdl:frame-rate) 60)
    (sdl:clear-display (color 0 colors))

    (setf *sy* nil
          *ey* nil)

    (if starting-pixels
	(eval starting-pixels)
        (draw-starting-pixels neighbourhood ruleset colors))

    (let ((evolve-fn (if (eql tag :number-of-neighbours)
                         #'evolve-from-prevgen
                         #'evolve))
          pause)
      (sdl:with-events ()
        (:quit-event () t)
        (:key-up-event (:key key)
                       (cond ((sdl:key= key :SDL-KEY-ESCAPE)
                              (sdl:push-quit-event))
                             ((sdl:key= key :SDL-KEY-SPACE)
                              (unless pause
                                (funcall evolve-fn
                                         neighbourhood ruleset colors tag))
                              (sdl:update-display))))
        (:idle ()
               (when (sdl:mouse-left-p)
                 (sdl:draw-pixel
                  (sdl:point :x (sdl:mouse-x)
                             :y (sdl:mouse-y))
                  :color (color 1 colors))
                 (setf *sy* (min *sy* (sdl:mouse-y)))
                 (setf *ey* (max *ey* (sdl:mouse-y))))
               (unless pause
                 (if auto
                     (funcall evolve-fn neighbourhood ruleset colors tag))
                 (if steps
                     (decf steps))
                 (if (eql steps 0)
                     (setf pause t)))
               (sdl:update-display))))))
