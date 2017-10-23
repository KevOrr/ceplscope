(asdf:defsystem #:ceplscope
  :description "An oscilloscope simulator written in Lisp and CEPL"
  :author "Kevin Orr"
  :depends-on (#:lisp-matrix
               #:cl-arrows
               #:slynk

               #:cepl
               #:cepl.sdl2
               #:livesupport
               #:cepl.skitter.sdl2
               #:dirt
               #:nineveh)
  :serial t
  :components ((:file "package")
               (:file "arrows")
               ;; (:file "matrix")
               (:file "util")
               (:file "ceplscope")
               (:file "asdfg")))
