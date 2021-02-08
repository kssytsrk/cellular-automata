;;;; ca.asd

(asdf:defsystem #:ca
  :description "Cellular automaton simulation"
  :author "kssytsrk at github"
  :license  "GNU GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:lispbuilder-sdl)
  :components ((:file "package")
               (:file "ca")
               (:file "world")
               (:file "variables")
               (:file "ui")
               (:file "start")))
