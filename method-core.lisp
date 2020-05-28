;;;; popts.lisp
(in-package :poly)

;;;; GLOBALS AND DIRECTIVES
(defparameter *workers-count* 4) ;; <- lparallel initialisation
(setf lparallel:*kernel* (lparallel:make-kernel *workers-count*))

;; (declaim (optimize (speed 3) (speed 0))) is moved to package.lisp

;; METHOD INTERFACE:
(defclass polynomial ()
  ((%approx-step :accessor approx-step))
  (:documentation "This is an interface to any polynomial."))

(defgeneric stable-p (this) ;; CACHE IT
  (:documentation "This is an interface to polynomial stability.
Returns t on nil, must cache it's result"))

(defgeneric vary (this x y new-x new-y)
  (:documentation "This is an interface to polynomial variation. 
Here x, y are symbols naming the variable parameters of this polynomial, new-x new-y are new values for this parameters.
Must return a new polynomial object, and leave the initial object unchanged"))

(defgeneric distance (this that x y)
  (:documentation "Computes the distance between 2 polynomials in a plane x y, if there is no unstable points."))

(defgeneric all-stable-distance (this x y)
  (:documentation "Computes distance from the nominal parameters to the nearest bound of stability area."))

(defgeneric radius (this x y)
  (:documentation "Calculates the stability radius of this polynomial"))

(defgeneric axis-range (this axis)
  (:documentation "Returns the range of variation for the given axis as a list"))

(defgeneric start (this axis)
  (:documentation "Returns the first value of the corresponding axis-range."))

(defgeneric end (this axis)
  (:documentation "Returns the last value of the corresponding axis-range."))

(defgeneric center (this x y)
  (:documentation "Returns (x-cnt y-cnt) pair, where x-cnt and y-cnt are nominal parameters."))

(defgeneric axis-pairs (this)
  (:documentation "Returns a list of axis combinatons by 2 in this polynomial."))

(defgeneric plot-to-file (this x y)
  (:documentation "Plots the stability area of the given polynomial in the given plane XY, returns the filename."))

;;;; METHOD HIGH-LEVEL PART OF THE IMPLEMENTATION:

(defmethod robust-radius ((this polynomial))
  "Calculates robust radius of the given polynomial."
  (->> (axis-pairs this)
    (lp:pmapcar (lambda (comb) ;; should be parallel
		  (radius this
			  (car  comb) ;; car is x, cadr is y
			  (cadr comb))))
    (apply #'min)))

(defmethod radius ((this polynomial) x y)
  (-<>>
      (lp:pmapcan (lambda (var-x)
		   (loop for var-y in (axis-range this y) ;; <- collecting unstables
			 for current-poly = (vary this x y var-x var-y)
			 unless (stable-p current-poly)
			   collect (distance this current-poly x y)
			     into unstable-dists
			   finally (return unstable-dists)))
		  (axis-range this x))
    (if <>
	(apply #'min <>)
	(all-stable-distance this x y))))

(defmethod plot-to-file ((this polynomial) x y)
  (flet ((format-datapoints () ;; must write x y stabiliy in order
	   (loop for var-x in (axis-range this x) do
	     (loop for var-y in (axis-range this y)
		   do (->> (vary this x y var-x var-y)
			(stable-p)
			(bool-to-int)
			(format t "~&~a ~a ~a" var-x var-y))))))
    (let* ((x-start (start this x))
	   (x-end   (end this x))
	   (y-start (start this y))
	   (y-end   (end this y))
	   (output-file-name (format nil "plot_~a_from:~a_to:~a_~a_from:~a_to:~a_by:~a.png"
				     x x-start x-end
				     y y-start y-end
				     (approx-step this))))
      (with-plots (*standard-output* :debug t)
	(gp-setup :terminal '(pngcairo)
		  :output output-file-name)
	(gp :set :view 'map)
	(gp :unset :key)
	(gp :set :cbtics '("(\"unstable\" 0, \"stable\" 1)"))
	(gp :set :palette '("maxcolors 2"))
	(gp :set :palette '("defined (0 \"magenta\", 1 \"cyan\")"))
	(gp :set :xrange (list x-start x-end))
	(gp :set :yrange (list y-start y-end))
	(gp :set :xlabel (list (format nil "\"~A\"" x)))
	(gp :set :ylabel (list (format nil "\"~A\"" y)))
	(gp :set :size '("ratio 1"))
	(gp :set :tics '(out))
	(plot #'format-datapoints :with '(:image))
	(plot #'(lambda () (format t "~&~{~a ~}" (center this x y)))
	      :with '(:points :linestyle 4)))
      output-file-name)))
