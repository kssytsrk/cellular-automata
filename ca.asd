;;;; ca.asd

(asdf:defsystem #:ca
  :description "Cellular automaton simulation"
  :author "kssytsrk at github"
  :license  "GNU GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2 #:cl-opengl)
  :components ((:file "package")
               (:file "ca")
               (:file "world")
               (:file "ui")))
