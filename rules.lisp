(in-package #:ca)

(defun get-ruleset (n &optional (d 1))
  (when (and (>= n 0)
             (< n (expt 2 (case d
                            (1 8)
                            (2 32)
                            (t 0)))))
    (let ((states (map 'list
                       (lambda (char)
                         (parse-integer (string char)))
                       (case d
                         (1 (format nil "~8,'0b" n))
                         (2 (format nil "~32,'0b" n))))) ;; set this to 32 on 2d!!
          (patterns (reverse (loop for i from 0 below (case d (1 8) (2 32) (t 0))
                            collect (map 'list
                                         (lambda (char)
                                           (parse-integer (string char)))
                                         (case d
                                           (1 (format nil "~3,'0b" i))
                                           (2 (format nil "~5,'0b" i))))))))
      (mapcar #'cons patterns states))))

(defparameter *ruleset* nil)
