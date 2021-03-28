;; -*-lisp-*-
;;
;; utils.lisp

(in-package #:ca)

(defun assoc-rh (item alist &key (test #'eql))
  (cdr (assoc item alist :test test)))

(defun color (number colorset)
  (assoc-rh number colorset))

(defun decimal-to-base-n-list (number padding base)
  (map 'list
       (lambda (char)
         (parse-integer (string char) :radix base))
       (format nil
               (concatenate 'string
			    "~" (write-to-string base) ","
			    (write-to-string padding) ",'0R")
               number)))

(defun print-ca (ca w)
  (loop for i in ca
        for n from 1
        do (format t "~d" i)
        if (eql (mod n w) 0)
          do (format t "~&")))
