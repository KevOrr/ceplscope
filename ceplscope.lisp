(in-package #:ceplscope)

(defparameter *eps* 1e-6)
(defparameter *loop-start-time* 0.0)
(defparameter *blending* (make-blending-params
                          :source-rgb :src-alpha
                          :destination-rgb :one
                          :mode-rgb :func-add))

(defun-g vert ((pnt :vec2) (vidx :float))
  (values (vec4 0.0) pnt vidx))

(defmacro-g -> (&body body)
  (macroexpand `(cl-arrows:-> ,@body)))

(defun-g geom ((pnts (:vec2 2)) (vidxs (:float 2))
               &uniform (uscale :float) (usize :float))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 4))
  (macrolet ((point (start end vidx idx)
               `(let* ((dir (- ,end ,start))
                       (tang-sign ,(if (>= idx 2) 1 -1))
                       (norm-sign ,(if (zerop (mod idx 2)) 1 -1))
                       (tang (normalize dir))
                       (norm (vec2 (- (y tang)) (x tang))))

                  (emit ()
                        (vec4 (-> (+ (* tang tang-sign) (* norm norm-sign))       ; base direction
                                  (* usize)          ; scaled segment quad
                                  (+ ,(if (>= idx 2) ; translate to current pos
                                          end
                                          start))
                                  (* uscale)) ; global scaling
                              0.0
                              1.0)

                        ,vidx
                        (v! ,(if (>= idx 2.0)
                                 `(- usize)
                                 `(+ (length dir) usize))
                            (* norm-sign usize))
                        (length dir))

                  (values))))

    (point (aref pnts 0) (aref pnts 1) (aref vidxs 0) 0)
    (point (aref pnts 0) (aref pnts 1) (aref vidxs 0) 1)
    (point (aref pnts 0) (aref pnts 1) (aref vidxs 1) 2)
    (point (aref pnts 0) (aref pnts 1) (aref vidxs 1) 3)

    (end-primitive)
    (values)))

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

(defun-g frag ((idx :float) (xy :vec2) (len :float)
               &uniform (usize :float) (uintensity :float))
  (let ((sigma (/ usize 4.0))
        (afterglow (smoothstep 0.0 0.33 (/ idx 2048.0))))

    (if nil
        ;; alpha = exp(-pow(length(xy),2.0)/(2.0*sigma*sigma))/2.0/sqrt(uSize);
        (let ((alpha (-> (length xy)
                         (pow 2.0)
                         (-)
                         (/ 2.0 sigma sigma)
                         (exp)
                         (/ 2.0 (sqrt usize)))

                     ;; (/ (exp (/ (- (pow (length xy) 2.0))
                     ;;            (* 2.0 sigma sigma)))
                     ;;    2.0
                     ;;    (sqrt usize))
                     ))
          (vec4 (/ 1.0 32.0) 1.0 (/ 1.0 32.0) (* alpha afterglow uintensity))
          )

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
                            usize))

                     ;; (* (- (erf (/ (x xy) #.(sqrt 2) sigma))
                     ;;       (erf (/ (- (x xy) len) #.(sqrt 2) sigma)))
                     ;;    (* usize
                     ;;       (/ (exp (/ (* (- (y xy)) (x xy))
                     ;;                  2.0
                     ;;                  sigma
                     ;;                  sigma))
                     ;;          2.0
                     ;;          len)))
                     ))
          (vec4 (/ 1.0 32.0) 1.0 (/ 1.0 32.0) (* alpha afterglow uintensity))
          ))))

(def-g-> scope (:line-strip)
  :vertex (vert :vec2 :float)
  :geometry (geom (:vec2 2) (:float 2))
  :fragment (frag :float :vec2 :float))

(defun make-curve (time &optional (trail 400) (t-factor (/ 1 1.0)))
  (let ((arr (make-gpu-array
              (loop :for i :downfrom time
                    :repeat trail
                    :collect (v! (cos (* i 3.0 t-factor)) (sin (* 2.0 i t-factor))))
              ;; (list (v! -1 0)
              ;;       (v! 0 1)
              ;;       (v! 1 0)
              ;;       (v! 0 -1)
              ;;       (v! -1 0))
              :element-type :vec2
              :dimensions trail
              ))
        (idxs (make-gpu-array
               (loop :for i := 2048.0
                     :repeat trail
                     :collect i)
               :element-type :float
               :dimensions trail
               )))
    (make-buffer-stream (list arr idxs) :primitive :line-strip)))

(defun step-scope ()
  (let* ((time (- (get-internal-real-time) *loop-start-time*))
         (curve (make-curve time)))
    (with-blending *blending*
      (clear)
      (map-g #'scope curve :uscale 0.8 :usize 0.02 :uintensity 40.0)
      ;; (map-g #'scope (make-buffer-stream
      ;;                 (make-gpu-array
      ;;                  (list (v! -1.0 0.0)
      ;;                        (v! 0.0 1.0)
      ;;                        (v! 1.0 0.0)
      ;;                        (v! 0.0 -1.0)
      ;;                        (v! -1.0 0.0))
      ;;                  :element-type :vec2)
      ;;                 :primitive :line-strip)
      ;;        :uscale 1.0 :usize 0.02 :uintensity 0.5)
      (swap))))

(def-simple-main-loop run-scope
    (:on-start (lambda ()
                 (print "strating")
                 (setf *loop-start-time* (get-internal-real-time))
                 (slynk-mrepl::send-prompt
                  (find (bt:current-thread) (slynk::channels)
                        :key #'slynk::channel-thread))))
  (step-scope))
