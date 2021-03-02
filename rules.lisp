;;;; -*-lisp-*-
;;;;
;;;; rules.lisp

(in-package #:ca)

(defun transition-rule (cells)
  (case *ruleset*
    (:game-of-life (game-of-life-transition-rule cells))
    (:wireworld    (wireworld-transition-rule cells))
    (t             (if (hash-table-p *ruleset*)
                       (gethash cells *ruleset*)
                       :wrong-ruleset-name))))

(defun game-of-life-transition-rule (cells)
  (let ((cell (first cells))
        (other-cells (reduce #'+ cells)))
    (if (or (and (eql cell 1)
                 (= other-cells 2))
            (= other-cells 3))
        1
        0)))

(defun wireworld-transition-rule (cells)
  (let ((cell (first cells))
        (other-cells (rest cells)))
    (case cell
      (0 0)
      ((1 2) (1+ cell))
      (3 (loop for i in other-cells
               with electron-heads = 0
               do (if (eql i 1)
                      (incf electron-heads))
               finally
                  (return (case electron-heads
                            ((1 2) 1)
                            (t 3))))))))

(defun decimal-to-binary-list (number padding)
  (map 'list
       (lambda (char)
         (parse-integer (string char)))
       (format nil
               (concatenate 'string "~" (format nil "~a" padding) ",'0b")
               number)))

(defun ruleset (n)
  (let ((max-pwr (case *neighbourhood*
                   (:elementary 8)
                   (:neumann 32)
                   (:moore 512)
                   (t 0)))
        (ruleset (make-hash-table :test #'equal)))
    (when (and (>= n 0) (< n (expt 2 max-pwr)))
      (let ((states (decimal-to-binary-list n max-pwr))
            (patterns (reverse
                       (loop for i from 0 below max-pwr
                             collect (decimal-to-binary-list i
                                                             (case *neighbourhood*
                                                               (:elementary 3)
                                                               (:neumann 5)
                                                               (:moore 9)))))))
        (mapcar (lambda (pattern state)
                    (setf (gethash pattern ruleset)
                          state))
                patterns states)
        ruleset))))

(defun draw-starting-pixels ()
  (cond ((eql *neighbourhood* :elementary)
	 (sdl:draw-pixel (sdl:point :x (/ *window-width* 2) :y 0)
			 :color (color 1)))
	((eql *ruleset* :game-of-life)
	 (sdl:draw-line-* (- (/ *window-width* 2) 10)
			  (/ *window-height* 2)
			  (+ (/ *window-width* 2) 10)
			  (/ *window-height* 2)
			  :color (color 1)))
	((eql *ruleset* :wireworld)
	 (sdl:draw-line-*  10 10  40            10 :color (ca::color 3))
	 (sdl:draw-line-*   0 11   9            11 :color (ca::color 3))
	 (sdl:draw-line-*  10 12  40            12 :color (ca::color 3))
	 (sdl:draw-line-*  40 11 *window-width* 11 :color (ca::color 3))
	 (sdl:draw-pixel-* 50 11                   :color (ca::color 1)))
	(t (sdl:draw-pixel (sdl:point :x (/ *window-width* 2)
				      :y (/ *window-height* 2))
			   :color (color 1)))))
