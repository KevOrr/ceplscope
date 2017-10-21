(in-package #:ceplscope)

(defparameter *intensity* 15.0)
(defparameter *scaling* 0.8)
(defparameter *weight* 0.02)

(defparameter *eps* 1e-6)
(defparameter *loop-start-time* 0.0)
(defparameter *blending* (make-blending-params
                          :source-rgb :src-alpha
                          :destination-rgb :one
                          :mode-rgb :func-add))


(defmacro-g -> (&body body)
  (macroexpand `(cl-arrows:-> ,@body)))

(defun-g gaussian ((x :float) (sigma :float))
  (/ (exp (/ (- (* x x))
             (* 2.0 sigma sigma)))
     (* sigma #.(sqrt (* 2 pi)))))

(defun-g erf ((x :float))
  (let ((s (sign x))
        (a (abs x)))
    (setf x (-> a
                (* a 0.078108)
                (+ 0.230389)
                (* a)
                (+ 0.278393)
                (* a)
                (+ 1))
          x (* x x)
          x (* x x))
    (- s (/ s x))))


(defun-g vert ((pnt :vec2) (glow :float))
  (values (vec4 0.0) pnt glow))

(defun-g geom ((pnts (:vec2 2)) (vglows (:float 2))
               &uniform (uscale :float) (usize :float))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 4))
  (macrolet ((point (start end glow idx)
               `(let* ((dir (- ,end ,start))
                       (tang-sign ,(if (>= idx 2) 1 -1))
                       (norm-sign ,(if (zerop (mod idx 2)) 1 -1))
                       (tang (normalize dir))
                       (norm (vec2 (- (y tang)) (x tang))))

                  (emit ()
                        (vec4 (-> (+ (* 0.0 tang tang-sign) (* norm norm-sign))       ; base direction
                                  (* usize)          ; scaled segment quad
                                  (+ ,(if (>= idx 2) ; translate to current pos
                                          end
                                          start))
                                  (* uscale)) ; global scaling
                              0.0
                              1.0)

                        ,glow
                        (v! ,(if (>= idx 2.0)
                                 `(+ usize)
                                 `(- (length dir) usize))
                            (* norm-sign usize))
                        (length dir))

                  (values))))

    (point (aref pnts 0) (aref pnts 1) (aref vglows 0) 0)
    (point (aref pnts 0) (aref pnts 1) (aref vglows 0) 1)
    (point (aref pnts 0) (aref pnts 1) (aref vglows 1) 2)
    (point (aref pnts 0) (aref pnts 1) (aref vglows 1) 3)

    (end-primitive)
    (values)))

(defun-g frag ((idx :float) (xy :vec2) (len :float)
               &uniform (usize :float) (uintensity :float))
  (let ((sigma (/ usize 4.0))
        (afterglow (smoothstep 0.0 0.33 (/ idx 2048.0))))

    (if (< len *eps*)
        ;; alpha = exp(-pow(length(xy),2.0)/(2.0*sigma*sigma))/2.0/sqrt(uSize);
        (let ((alpha (-> (length xy)
                         (pow 2.0)
                         (-)
                         (/ 2.0 sigma sigma)
                         (exp)
                         (/ 2.0 (sqrt usize)))))
          (vec4 (/ 1.0 32.0) 1.0 (/ 1.0 32.0) (* alpha afterglow uintensity)))

        ;; alpha = erf(xy.x/SQRT2/sigma) - erf((xy.x-len)/SQRT2/sigma);
        ;; alpha *= exp(-xy.y*xy.y/(2.0*sigma*sigma))/2.0/len*uSize;
        (let ((alpha (-> (- (-> (/ (x xy) #.(sqrt 2) sigma)
                                (erf))
                            (-> (- (x xy) len)
                                (/ #.(sqrt 2) sigma)
                                (erf)))
                         (* (-> (y xy)
                                (pow 2.0)
                                (-)
                                (/ (* 2.0 sigma sigma))
                                (exp)
                                (/ 2.0 len))
                            usize))))
          (vec4 (/ 1.0 32.0) 1.0 (/ 1.0 32.0) (* 0.5 alpha afterglow uintensity))))))

(def-g-> scope (:line-strip)
  :vertex (vert :vec2 :float)
  :geometry (geom (:vec2 2) (:float 2))
  :fragment (frag :float :vec2 :float))

(defun make-curve (time &optional (trail 1000) (t-factor 0.05))
  (let ((arr (make-gpu-array
              (loop :for i :downfrom time
                    :repeat trail
                    :collect (v! (cos (+ (* 3.0 i t-factor)
                                         (* 0.000 i t-factor)))
                                 (sin (+ (* 5.0 i t-factor)
                                         (* 0.002 i t-factor)))))
              :element-type :vec2
              :dimensions trail
              ))
        (glows (make-gpu-array
                (loop :for i :below trail
                      :collect (* (expt (- trail i) 2.0)
                                  (/ 500.0 (expt trail 2.0))))
                :element-type :float
                :dimensions trail
                )))
    (make-buffer-stream (list arr glows) :primitive :line-strip)))

(defun step-scope ()
  (let* ((time (- (get-internal-real-time) *loop-start-time*))
         (curve (make-curve time)))
    (with-blending *blending*
      (clear)
      (map-g #'scope curve :uscale *scaling*
                           :usize *weight*
                           :uintensity *intensity*)
      (swap))))

(def-simple-main-loop run-scope
    (:on-start (lambda ()
                 (print "strating")
                 (setf *loop-start-time* (get-internal-real-time))
                 (gl:clear-color 0.0 0.0 0.0 0.0)
                 (gl:depth-func :lequal)
                 (slynk-mrepl::send-prompt
                  (find (bt:current-thread) (slynk::channels)
                        :key #'slynk::channel-thread))))
  (let ((start (get-internal-real-time)))
    (step-scope)
    (sleep (max 0.0 (- (/ 1.0 60.0)
                       (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second))))))
