(defpackage ceplscope.arrows
  (:use #:cl #:cepl)
  (:export #:->
           #:->>
           #:-<>))

(defpackage ceplscope
  (:use #:cl
        #:ceplscope.arrows

        #:cepl
        #:vari
        #:rtg-math
        #:livesupport
        #:nineveh))
