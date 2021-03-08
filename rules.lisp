;;;; -*-lisp-*-
;;;;
;;;; rules.lisp

(in-package #:ca)

(defun transition-rule (cells ruleset &optional tag)
  (case ruleset
    (:game-of-life (game-of-life-transition-rule cells))
    (:wireworld    (wireworld-transition-rule cells))
    (t             (if (hash-table-p ruleset)
		       (let ((fn
                               (case tag
                                 (:totalistic #'totalistic-transition-rule)
                                 (:number-of-neighbours #'number-of-neighbours-transition-rule)
                                 (t #'-transition-rule))))
                         (funcall fn
                                  cells ruleset))
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

(defun totalistic-transition-rule (cells ruleset)
  (gethash (/ (reduce #'+ cells)
              (length cells))
           ruleset))

(defun number-of-neighbours-transition-rule (cells ruleset)
  (gethash (list (first cells) (reduce #'+ (rest cells)))
           ruleset))

(defun -transition-rule (cells ruleset)
  (gethash cells
           ruleset))

(defun ruleset (n neighbourhood)
  (let ((max-pwr (case neighbourhood
                   (:elementary 8)
                   (:neumann 32)
                   (:moore 512)
                   (t 0)))
        (ruleset (make-hash-table :test #'equal)))
    (when (and (>= n 0) (< n (expt 2 max-pwr)))
      (let ((states (decimal-to-base-n-list n max-pwr 2))
            (patterns (reverse
                       (loop for i from 0 below max-pwr
                             collect (decimal-to-base-n-list i
                                                             (case neighbourhood
                                                               (:elementary 3)
                                                               (:neumann 5)
                                                               (:moore 9))
							     2)))))
        (mapcar (lambda (pattern state)
		  (setf (gethash pattern ruleset)
                        state))
                patterns states)
        ruleset))))

(defun totalistic-ruleset (n neighbourhood &optional (color-number 3))
  (let* ((nb (case neighbourhood
               (:elementary 3)
               (:neumann 5)
               (:moore 9)
               (t 0)))
         (max-pwr (+ 1 (* nb (1- color-number))))
         (ruleset (make-hash-table :test #'equal)))
    (when (and (>= n 0) (< n (expt color-number max-pwr)))
      (let ((states (decimal-to-base-n-list n max-pwr color-number))
            (patterns (reverse
                       (loop for i from 0 to (1- color-number) by (/ 1 nb)
                             collect i))))
        (mapcar (lambda (pattern state)
                  (setf (gethash pattern ruleset)
                        state))
                patterns states)
	ruleset))))

(defun number-of-neighbours-ruleset (n neighbourhood)
  (let ((max (case neighbourhood
               (:elementary 3)
               (:neumann 5)
               (:moore 9)
               (t 0)))
        (ruleset (make-hash-table :test #'equal)))
    (when (and (>= n 0) (< n (expt 2 (* 2 max))))
      (let ((states (decimal-to-base-n-list n (* 2 max) 2))
            (patterns (reverse
                       (loop for i from 0 below max
                             append (list (list 0 i)
					  (list 1 i))))))
        (mapcar (lambda (pattern state)
                  (setf (gethash pattern ruleset)
                        state))
                patterns states)
        ruleset))))
