(asdf:defsystem #:ceplscope
  :description "An oscilloscope simulator written in Lisp and CEPL"
  :author "Kevin Orr"
  :depends-on (#:cepl
               #:cepl.sdl2
               #:slynk
               #:livesupport
               #:cepl.skitter.sdl2
               #:dirt
               #:nineveh
               #:cl-arrows)
  :serial t
  :components ((:file "package")
               (:file "ceplscope")))
