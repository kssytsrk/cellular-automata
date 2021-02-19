(in-package #:ca)

(defun get-transition-rule (cells)
  (case *ruleset*
    (:game-of-life (get-game-of-life-transition-rule cells))
    (t (assoc-rh cells *ruleset* :test #'equal))))

(defun get-game-of-life-transition-rule (cells)
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
               (concatenate 'string
                            "~"
                            (format nil "~a" padding)
                            ",'0b")
               number)))

(defun get-ruleset (n)
  (let ((max-pwr (case *neighbourhood*
                   (:1d 8)
                   (:neumann 32)
                   (:moore 512)
                   (t 0))))
    (when (and (>= n 0)
                     (< n (expt 2 max-pwr)))
            (let ((states (decimal-to-binary-list n max-pwr))
                  (patterns (reverse
                             (loop for i from 0 below max-pwr
                                   collect (decimal-to-binary-list i
                                                                   (case *neighbourhood*
                                                                     (:1d 3)
                                                                     (:neumann 5)
                                                                     (:moore 9)))))))
              (reverse (mapcar #'cons patterns states))))))
