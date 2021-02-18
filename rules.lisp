(in-package #:ca)

(defun get-transition-rule (cells)
  (assoc-rh cells *ruleset* :test #'equal))

(defun get-game-of-life-transition-rule (cells)
  (let ((cell (first cells))
        (other-cells (reduce #'+ cells)))
    (if (or (and (eql cell 1)
                 (= other-cells 2))
            (= other-cells 3))
        1
        0)))

(defun get-ruleset (n &optional (d 1))
  (when (and (>= n 0)
             (< n (expt 2 (case d
                            (1 8)
                            (:neumann 32)
                            (:moore 512)
                            (t 0)))))
    (let ((states (map 'list
                       (lambda (char)
                         (parse-integer (string char)))
                       (case d
                         (1 (format nil "~8,'0b" n))
                         (:neumann (format nil "~32,'0b" n))
                         (:moore (format nil "~512,'0b" n)))))
          (patterns (reverse
                     (loop for i from 0 below (case d
                                                (1 8)
                                                (:neumann 32)
                                                (:moore 512)
                                                (t 0))
                           collect (map 'list
                                        (lambda (char)
                                          (parse-integer (string char)))
                                        (case d
                                          (1 (format nil "~3,'0b" i))
                                          (:neumann (format nil "~5,'0b" i))
                                          (:moore (format nil "~9,'0b" i))))))))
      (reverse (mapcar #'cons patterns states)))))

(defparameter *ruleset* nil)

(defparameter *n-states* 0)
