(in-package ceplscope.matrix)

(defun dcos (number) (coerce (cos number) 'double-float))
(defun dsin (number) (coerce (cos number) 'double-float))

(defun make-transform-matrix (reflect shear rotate scale translate))
  

(defun make-transform-matrix (reflect shear rotate scale translate)
  (let* ((*default-implementation* :lisp-array)
         (*default-element-type* 'double-float)
         (scale (etypecase scale
                  ((cons float float) scale)
                  (float (cons scale scale))))
         (reflect-x (if (car reflect) -1d0 1d0))
         (reflect-y (if (cdr reflect) -1d0 1d0))
         (reflect-mat (make-matrix 4 4 :initial-contents
                                   `((,reflect-x 0d0 0d0 0d0)
                                     (0d0 ,reflect-y 0d0 0d0)
                                     (0d0 0d0 1d0 0d0)
                                     (0d0 0d0 0d0 1d0))))
         (shear-mat (make-matrix 4 4 :initial-contents
                                 `((1d0 ,(car shear) 0d0 0d0)
                                   (,(cdr shear) 1d0 0d0 0d0)
                                   (0d0 0d0 1d0 0d0)
                                   (0d0 0d0 0d0 1d0))))
         (rotate-mat (make-matrix 4 4 :initial-contents
                                  `((,(dcos rotate) ,(- (dsin rotate)) 0d0 0d0)
                                    (,(dsin rotate) ,(dcos rotate) 0d0 0d0)
                                    (0d0 0d0 1d0 0d0)
                                    (0d0 0d0 0d0 1d0))))
         (scale-mat (make-matrix 4 4 :initial-contents
                                 `((,(car scale) 0d0 0d0 0d0)
                                   (0d0 ,(cdr scale) 0d0 0d0)
                                   (0d0 0d0 1d0 0d0)
                                   (0d0 0d0 0d0 1d0))))
         (translate-mat (make-matrix 4 4 :initial-contents
                                     `((1d0 0d0 ,(car translate) 0d0)
                                       (0d0 1d0 ,(cdr translate) 0d0)
                                       (0d0 0d0 1d0 0d0)
                                       (0d0 0d0 0d0 1d0)))))
    (cl-arrows:->> reflect-mat
                   (m* shear-mat)
                   (m* rotate-mat)
                   (m* scale-mat)
                   (m* translate-mat)
                   (data))))
