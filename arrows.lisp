(in-package :ceplscope.arrows)

(defmacro-g -> (&body body)
  (macroexpand `(cl-arrows:-> ,@body)))

(defmacro-g -<> (&body body)
  (macroexpand `(cl-arrows:-<> ,@body)))

(defmacro-g ->> (&body body)
  (macroexpand `(cl-arrows:->> ,@body)))

(defmacro-g -<> (&body body)
  (macroexpand `(cl-arrows:-<> ,@body)))

