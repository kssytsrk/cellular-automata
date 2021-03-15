;;;; -*-lisp-*-
;;;;
;;;; pixels.lisp
;;;; Helper functions for evolution functions that have to do with pixel
;;;; manipulation (read/write)

(in-package #:ca)

(defun check-and-fix-bounds (&key x y)
  (and x
       (cond ((>= x (window-width)) (setf x 0))
             ((< x 0) (setf x (1- (window-width))))))
  (and y
       (cond ((>= y (window-height)) (setf y 0))
             ((< y 0) (setf y (1- (window-height))))))
  (if (and x y) (values x y) (or x y)))

(defun pixel-value (x y colorset)
  (multiple-value-bind (x y) (check-and-fix-bounds :x x :y y)
    (or (gethash (list x y) *cached-values*)
        (setf (gethash (list x y) *cached-values*)
              (let ((surface-fp (sdl:fp sdl:*default-display*)))
                (sdl:with-pixel (pix surface-fp)
                  (car (rassoc (unpack-color (sdl:read-pixel pix x y))
                               colorset :test #'sdl:color=))))))))

(defun elementary-neighbours-values (x y colorset)
  (list (pixel-value (1- x) (1- y) colorset)
        (pixel-value x      (1- y) colorset)
        (pixel-value (1+ x) (1- y) colorset)))

(defun neumann-neighbours-values (x y colorset)
  (list (pixel-value     x     y  colorset)  ; v
        (pixel-value (1+ x)    y  colorset)  ; v0
        (pixel-value     x (1+ y) colorset)  ; v1
        (pixel-value (1- x) y     colorset)  ; v2
        (pixel-value     x (1- y) colorset)  ; v3
        ))

(defun moore-neighbours-values (x y colorset)
  (list (pixel-value     x       y colorset) ; v
        (pixel-value (1+ x)      y colorset) ; v0
        (pixel-value     x  (1+ y) colorset) ; v1
        (pixel-value (1- x)      y colorset) ; v2
        (pixel-value     x  (1- y) colorset) ; v3
        (pixel-value (1+ x) (1+ y) colorset) ; v4
        (pixel-value (1- x) (1+ y) colorset) ; v5
        (pixel-value (1- x) (1- y) colorset) ; v6
        (pixel-value (1+ x) (1- y) colorset) ; v7
        ))

(defun neighbours-values (neighbourhood x y colorset)
  (funcall (case neighbourhood
             (:elementary #'elementary-neighbours-values)
             (:neumann    #'neumann-neighbours-values)
             (:moore      #'moore-neighbours-values))
           x y colorset))

(defun color-for-pixel (ruleset neighbourhood colorset x y &key (use-cache t))
  (let* ((cells (neighbours-values neighbourhood x y colorset))
         (new-value (transition-rule cells ruleset)))
    (if use-cache
        (setf (gethash (list x y) *cached-values*) new-value))
    (when (not (eql new-value 0))
      (let ((1-y (check-and-fix-bounds :y (1- y)))
            (1+y (check-and-fix-bounds :y (1+ y))))
        (if *sy*
            (setf *sy* (min *sy* 1-y 1+y))
            (setf *sy* (min 1-y 1+y)))
	(if (and (eql neighbourhood :elementary)
		 (< *sy* 1))
	    (setf *sy* 1))
        (if *ey*
            (setf *ey* (max *ey* 1-y 1+y))
            (setf *ey* (max 1-y 1+y)))))
    (unless (numberp new-value)
      (error "~&Pixel at x ~a, y ~a, current value: ~a new value: ~a
                ~&Neighbourhood: ~a
                ~&Ruleset ~a"
             x y
             (pixel-value x y colorset)
             new-value
             neighbourhood ruleset))
    (color new-value colorset)))

;;;; only valid within with-pixel/s
(defmacro write-pixel (pix x y color)
  `(sdl:write-pixel
    ,pix ,x ,y
    (apply #'sdl-cffi::sdl-map-rgba
	   (concatenate 'list
			(list (sdl-base:pixel-format surface-fp))
			(sdl:fp ,color)))))

(defun draw-starting-pixels (neighbourhood ruleset colorset)
  (cond ((eql neighbourhood :elementary)
	 (sdl:draw-pixel (sdl:point :x (/ (window-width) 2) :y 0)
			 :color (color 1 colorset)))
	((eql ruleset :game-of-life)
	 (sdl:draw-line-* (- (/ (window-width) 2) 10)
			  (/ (window-height) 2)
			  (+ (window-width) 10)
			  (/ (window-height) 2)
			  :color (color 1 colorset)))
	((eql ruleset :wireworld)
	 (sdl:draw-line-*  10 10  40                            10
                           :color (color 3 colorset))
	 (sdl:draw-line-*   0 11   9                            11
                           :color (color 3 colorset))
	 (sdl:draw-line-*  10 12  40                            12
                           :color (color 3 colorset))
	 (sdl:draw-line-*  40 11 (window-width) 11
                           :color (color 3 colorset))
	 (sdl:draw-pixel-* 50 11
                           :color (color 1 colorset)))
	(t (sdl:draw-pixel (sdl:point :x (/ (window-width) 2)
				      :y (/ (window-height) 2))
			   :color (color 1 colorset)))))
