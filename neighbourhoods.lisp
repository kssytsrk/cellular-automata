;;;; -*-lisp-*-
;;;;
;;;; neighbourhoods.lisp

(in-package #:ca)

(defvar *neumann-nb* '((0 0) (1 0) (0 1) (-1 0) (0 -1)))
(defvar *elementary-nb* '((-1) (0) (1)))
(defvar *moore-nb* '((0 0) (1 0) (0 1)
                     (-1 0) (0 -1) (1 1)
                     (-1 1) (-1 -1) (1 -1)))
