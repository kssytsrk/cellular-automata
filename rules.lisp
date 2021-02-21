;;;; -*-lisp-*-
;;;;
;;;; rules.lisp

(in-package #:ca)

(defun transition-rule (cells)
  (case *ruleset*
    (:game-of-life (game-of-life-transition-rule cells))
    (t (gethash cells *ruleset*))))

(defun game-of-life-transition-rule (cells)
  (let ((cell (first cells))
        (other-cells (reduce #'+ cells)))
    (if (or (and (eql cell 1)
                 (= other-cells 2))
            (= other-cells 3))
        1
        0)))

(defun decimal-to-binary-list (number padding)
  (map 'list
       (lambda (char)
         (parse-integer (string char)))
       (format nil
               (concatenate 'string "~" (format nil "~a" padding) ",'0b")
               number)))

(defun ruleset (n)
  (let ((max-pwr (case *neighbourhood*
                   (:1d 8)
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
                                                               (:1d 3)
                                                               (:neumann 5)
                                                               (:moore 9)))))))
        (mapcar (lambda (pattern state)
                    (setf (gethash pattern ruleset)
                          state))
                patterns states)
        ruleset))))
