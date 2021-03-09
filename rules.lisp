;;;; -*-lisp-*-
;;;;
;;;; rules.lisp

(in-package #:ca)

(defun transition-rule (cells ruleset)
  (funcall (cdr (assoc-rh (car ruleset) *rules*
                          :test #'equal))
           cells (cdr ruleset)))

(defun game-of-life-transition-rule (cells ruleset)
  (declare (ignore ruleset))
  (let ((cell (first cells))
        (other-cells (reduce #'+ cells)))
    (if (or (and (eql cell 1)
                 (= other-cells 2))
            (= other-cells 3))
        1
        0)))

(defun wireworld-transition-rule (cells ruleset)
  (declare (ignore ruleset))
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

(defun neighbour-number-transition-rule (cells ruleset)
  (gethash (list (first cells) (reduce #'+ (rest cells)))
           ruleset))

(defun -transition-rule (cells ruleset)
  (gethash cells
           ruleset))

(defun ruleset (n neighbourhood possible-states)
  (declare (ignore possible-states))
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

(defun totalistic-ruleset (n neighbourhood possible-states)
  (let* ((nb (case neighbourhood
               (:elementary 3)
               (:neumann 5)
               (:moore 9)
               (t 0)))
         (max-pwr (+ 1 (* nb (1- possible-states))))
         (ruleset (make-hash-table :test #'equal)))
    (when (and (>= n 0) (< n (expt possible-states max-pwr)))
      (let ((states (decimal-to-base-n-list n max-pwr possible-states))
            (patterns (reverse
                       (loop for i from 0 to (1- possible-states) by (/ 1 nb)
                             collect i))))
        (mapcar (lambda (pattern state)
                  (setf (gethash pattern ruleset)
                        state))
                patterns states)
	ruleset))))

(defun neighbour-number-ruleset (n neighbourhood possible-states)
  (declare (ignore possible-states))
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

(defparameter *rules*
  (list
   (cons :game-of-life         (cons nil #'game-of-life-transition-rule))
   (cons :wireworld            (cons nil #'wireworld-transition-rule))
   (cons :totalistic           (cons #'totalistic-ruleset
                                     #'totalistic-transition-rule))
   (cons :neighbour-number     (cons #'neighbour-number-ruleset
                                     #'neighbour-number-transition-rule))
   (cons :normal               (cons #'ruleset #'-transition-rule))))
