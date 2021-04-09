;;;; -*-lisp-*-
;;;;
;;;; neighbourhoods.lisp

(in-package #:ca)

(defun neighbourhood-variable (nb-name)
  "Returns the neighbourhood's variable *<nb-name>-NB*."
  (eval (find-symbol (concatenate 'string
                                  "*"
                                  (symbol-name nb-name)
                                  "-NB*")
                     'ca)))

(defun cell-neighbourhoods (neighbourhoods bounds)
  "Returns all cells' neighbourhoods in a cellular automaton
of dimensions specified by BOUNDS."
  (mapcar (lambda (cell)
            (mapcar (lambda (neighbourhood)
                      (point-to-index
                       (fix-bounds bounds (mapcar #'+ cell neighbourhood))
                       bounds))
                    neighbourhoods))
          (get-all-cells bounds)))

(defun cell-neighbourhood (cells neighbourhood)
  "Returns the values of the cells in NEIGHBOURHOOD."
  (mapcar #'(lambda (index) (elt cells index))
          neighbourhood))

(defvar *neumann-nb* '((0 0) (1 0) (0 1) (-1 0) (0 -1)))
(defvar *elementary-nb* '((-1) (0) (1)))
(defvar *moore-nb* '((0 0) (1 0) (0 1)
                     (-1 0) (0 -1) (1 1)
                     (-1 1) (-1 -1) (1 -1)))
