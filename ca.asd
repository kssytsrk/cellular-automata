;;;; -*-lisp-*-
;;;;
;;;; ca.asd

(asdf:defsystem #:ca
  :description "Cellular automaton simulation"
  :author "kssytsrk at github"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:lispbuilder-sdl)
  :components ((:file "package")
               (:file "utils")
               (:file "colors")
               (:file "rules")
               (:file "ca")
               (:file "ui")))
