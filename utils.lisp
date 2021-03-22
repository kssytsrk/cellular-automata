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
