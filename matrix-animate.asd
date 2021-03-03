;;;; matrix-animate.asd

(asdf:defsystem #:matrix-animate
  :description "Describe matrix-animate here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2 #:sdl2-ttf #:parse-number #:cl-ppcre)
  :components ((:file "package")
               (:file "matrix-animate")))
