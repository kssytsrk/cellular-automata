;;;; ca.asd

(asdf:defsystem #:ca
  :description "Describe ca here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2 #:cl-opengl)
  :components ((:file "package")
               (:file "ca")
               (:file "world")
               (:file "ui")))
