;;;; -*-lisp-*-
;;;;
;;;; rules.lisp

(in-package #:ca)

(defmacro transition-rule-fn (rule-name)
  "Returns the transition rule's function #'<rule-name>-TRANSITION-RULE."
  `(function ,(find-symbol (concatenate 'string
                                        (symbol-name rule-name)
                                        "-TRANSITION-RULE")
                           'ca)))

(defmacro ruleset-fn (rule-name)
  "Returns the ruleset creating function #'<rule-name>-RULESET."
  `(function ,(find-symbol (concatenate 'string
                                        (symbol-name rule-name)
                                        "-RULESET")
                           'ca)))

(defun transition-rule (cells ruleset)
  "Calls the appropriate transition rule."
  (funcall (eval `(transition-rule-fn ,(car (car ruleset))))
           cells (cdr ruleset)))

(defun game-of-life-ruleset (n neighbourhood possible-states)
  (declare (ignore n neighbourhood possible-states)) t)

(defun game-of-life-transition-rule (cells ruleset)
  (declare (ignore ruleset))
  (let ((cell (first cells))
        (other-cells (reduce #'+ cells)))
    (if (or (and (eql cell 1)
                 (= other-cells 2))
            (= other-cells 3))
        1
        0)))

(defun wireworld-ruleset (n neighbourhood possible-states)
  (declare (ignore n neighbourhood possible-states)) t)

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

(defun normal-transition-rule (cells ruleset)
  (gethash cells
           ruleset))

(defun normal-ruleset (n neighbourhood possible-states)
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
