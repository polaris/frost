(defpackage :FROST
  (:use :CL :PNG)
  (:export :RENDER))

(in-package frost)

(defstruct (point)
  x y z)

(defstruct (surface)
  color)

(defstruct (sphere (:include surface))
  radius center)

(defstruct (light)
  position)

(defstruct (scene)
  eye surfaces lights)

(defstruct (ray)
  origin direction)

(defun sq (x)
  (* x x))

(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun distance (p1 p2)
  (mag (- (point-x p1) (point-x p2))
       (- (point-y p1) (point-y p2))
       (- (point-z p1) (point-z p2))))

(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
	(unless (minusp disc)
	  (let ((discrt (sqrt disc)))
	    (min (/ (+ (- b) discrt) (* 2 a))
		 (/ (- (- b) discrt) (* 2 a))))))))

(defun put-pixel (canvas x y color)
  (setf (aref canvas y x 0) color))

(defun tracer (scn canvas width height res)
  (do ((y (- (/ height 2)) (+ y 1)))
      ((< (- (/ height 2) y) 1))
    (do ((x (- (/ width 2)) (+ x 1)))
	((< (- (/ width 2) x) 1))
      (put-pixel canvas 
		 (+ x (/ width 2))
		 (+ y (/ height 2))
		 (color-at scn (/ x res) (/ y res))))))

(defun dir-vector (p1 p2)
  (lm:normalise (lm:make-vector 3 :initial-elements (list (- (point-x p2) (point-x p1))
							  (- (point-y p2) (point-y p1))
							  (- (point-z p2) (point-z p1))))))

(defun color-at (scn x y)
  (let* ((vr (dir-vector (scene-eye scn) (make-point :x x :y y :z 0)))
	 (r (make-ray :origin (scene-eye scn) :direction vr)))
    (round (* (sendray scn r) 255))))

(defun sendray (scn r)
  (multiple-value-bind (s int) (first-hit scn r)
    (if s
	(let ((color 0))
	  (dolist (l (scene-lights scn))
	    (let ((vl (dir-vector (light-position l) int)))
	      (setf color (+ color (* (lambert s int vl) (surface-color s))))))
	  (min color 255))
	0)))

(defun first-hit (scn r)
  (let (surface hit dist)
    (dolist (s (scene-surfaces scn))
      (let ((h (intersect s r)))
	(when h
	  (let ((d (distance h (ray-origin r))))
	    (when (or (null dist)
		      (< d dist))
	      (setf surface s 
		    hit h 
		    dist d))))))
    (values surface hit)))

(defun lambert (s int vr)
  (let ((vn (normal s int)))
    (max 0 (+ (* (lm:x vr) (lm:x vn)) 
	      (* (lm:y vr) (lm:y vn))
	      (* (lm:z vr) (lm:z vn))))))

(defun create-sphere (x y z r c)
  (make-sphere :radius r
	       :center (make-point :x x :y y :z z)
	       :color c))

(defun intersect (s r)
  (funcall (typecase s 
	     (sphere #'sphere-intersect))
	   s r))

(defun sphere-intersect (s r)
  (let* ((c (sphere-center s))
	 (n (minroot (+ (sq (lm:x (ray-direction r))) (sq (lm:y (ray-direction r))) (sq (lm:z (ray-direction r))))
		     (* 2 (+ (* (- (point-x (ray-origin r)) (point-x c)) (lm:x (ray-direction r)))
			     (* (- (point-y (ray-origin r)) (point-y c)) (lm:y (ray-direction r)))
			     (* (- (point-z (ray-origin r)) (point-z c)) (lm:z (ray-direction r)))))
		     (+ (sq (- (point-x (ray-origin r)) (point-x c)))
			(sq (- (point-y (ray-origin r)) (point-y c)))
			(sq (- (point-z (ray-origin r)) (point-z c)))
			(- (sq (sphere-radius s)))))))
    (if n
	(make-point :x (+ (point-x (ray-origin r)) (* n (lm:x (ray-direction r))))
		    :y (+ (point-y (ray-origin r)) (* n (lm:y (ray-direction r))))
		    :z (+ (point-z (ray-origin r)) (* n (lm:z (ray-direction r))))))))

(defun normal (s pt)
  (funcall (typecase s
	     (sphere #'sphere-normal))
	   s pt))

(defun sphere-normal (s pt)
  (let* ((c (sphere-center s))
	 (vn (lm:make-vector 3 
			     :initial-elements (list (- (point-x c) (point-x pt))
						     (- (point-y c) (point-y pt))
						     (- (point-z c) (point-z pt))))))
    (lm:normalise vn)))

(defun init-scene ()
  (let ((srfcs nil)
	(lghts nil))
    (push (create-sphere 0 -300 -1200 200 .8) srfcs)
    (push (create-sphere -80 -150 -1200 200 .7) srfcs)
    (push (create-sphere 70 -100 -1200 200 .9) srfcs)
    (do ((x -2 (1+ x)))
	((> x 2))
      (do ((z 2 (1+ z)))
	  ((> z 7))
	(push (create-sphere (* x 200) 300 (* z -400) 50 .75) srfcs)))
    (push (make-light :position (make-point :x 1000 :y 0 :z 0)) lghts)
    (make-scene :eye (make-point :x 0 :y 0 :z 200) :surfaces srfcs :lights lghts)))

(defun render (pathname)
  (let* ((scn (init-scene))
	 (width 640) 
	 (height 480)
	 (res 3)
	 (result (png:make-image height width 1)))
    (tracer scn result width height res)
    (let ((p (open pathname
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :overwrite
		   :element-type '(unsigned-byte 8))))
      (png:encode result p)
      (close p))))