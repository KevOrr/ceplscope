(in-package :ceplscope)

(defparameter *scale* (v3:make 0.8 0.8 1.0))
(defparameter *rotate* 0.0)
(defparameter *translate* (v3:make 0.0 0.0 0.0))

(defparameter *usize* 0.005)
(defparameter *uintensity* 2.0)
(defparameter *uintensity-base* 0.2)
(defparameter *urgb* (v3:make (/ 1.0 32.0) 1.0 (/ 1.0 32.0)))

(defun-g asdfg-vert ((pos :vec2) (glow :float))
  (vec4 pos 0.0 1.0))

(defun-g asdfg-geom (&uniform (umat :mat4))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 4))
  (let* ((p0 (s~ (gl-position (aref gl-in 0)) :xy))
         (p1 (s~ (gl-position (aref gl-in 1)) :xy))
         (dir (- p1 p0))
         (len (length dir))
         (unit-dir (if (> len *eps*)
                       (/ dir len)
                       (vec2 1.0 0.0)))
         (unit-norm (vec2 (- (y unit-dir)) (x unit-dir)))
         (dir-margin (* unit-dir *usize*))
         (norm-margin (* unit-norm *usize*)))

    (macrolet ((emit-vert (start dir-sign norm-sign)
                 `(emit ()
                        (* umat
                           (vec4 (-> ,start
                                     (,dir-sign dir-margin)
                                     (,norm-sign norm-margin))
                                 0.0
                                 1.0))
                        p0
                        (vec3 ,(ecase dir-sign
                                 (- `(- *usize*))
                                 (+ `(+ len *usize*)))
                              (,norm-sign *usize*)
                              len))))
      (emit-vert p0 - +)
      (emit-vert p0 - -)
      (emit-vert p1 + +)
      (emit-vert p1 + -)
      (end-primitive)
      (values))))

(defun-g asdfg-frag ((start-point :vec2) (uv :vec3))
  (let* ((len (z uv))
         (xy (s~ uv :xy))
         (sigma (->> (* *usize* 40.0)
                     (+ 2.0)
                     (/ *usize*)))
         (alpha (-<> (- (normcdf (x xy) sigma)
                        (normcdf (- (x xy) len) sigma))
                     (* *usize*)
                     (* (exp (/ (pow (- (y xy)) 2)
                                2.0 sigma sigma)))
                     (/ 2.0 len)
                     (pow <> (- 1.0 *uintensity-base*))
                     (* *uintensity*))))
    (vec4 *urgb* alpha)))

(defpipeline-g asdfg-pipeline (:line-strip)
  :vertex (asdfg-vert :vec2 :float)
  :geometry (asdfg-geom)
  :fragment (asdfg-frag :vec2 :vec3))

(defun step-asdfg ()
  (let* ((time (- (get-internal-real-time) *loop-start-time*))
         (curve
           ;; (make-buffer-stream
            ;; (make-gpu-array
             ;; (list (v! -1.0 -0.37)
             ;;       (v! 1.0 -0.37)
             ;;       (v! 0.0 1.0)
             ;;       (v! -1.0 -0.37))
            ;;  :element-type :vec2)
            ;; :primitive :line-strip)
           (make-curve time)
           ))
    (with-blending *blending*
      (clear)
      (map-g #'asdfg-pipeline curve
             :umat (m4:* (m4:translation *translate*)
                         (m4:rotation-z *rotate*)
                         (m4:scale *scale*)))
      (swap))))

(def-simple-main-loop run-asdfg
    (:on-start (lambda ()
                 (setf *loop-start-time* (get-internal-real-time))
                 (gl:clear-color 0.0 0.0 0.0 0.0)
                 (gl:depth-func :lequal)
                 (slynk-mrepl::send-prompt
                  (find (bt:current-thread) (slynk::channels)
                        :key #'slynk::channel-thread))))
  (let ((start (get-internal-real-time)))
    (step-asdfg)
    (sleep (max 0.0 (- (/ 1.0 60.0)
                       (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second))))))
