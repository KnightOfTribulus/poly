(ql:quickload '(arrow-macros trivia alexandria lparallel str poly modf))

(in-package :poly)

(defmacro total-run-time (&body thunk)
  (al:with-gensyms (outstr results)
    `(-<>>
	 (with-output-to-string (,outstr)
	   (let ((*trace-output* ,outstr))
	     (setq ,results (multiple-value-list (time ,@thunk)))
	     (terpri ,outstr)))
       (str:split "real time")
       (cadr)
       (str:split "system)")
       (car)
       (str:words)
       (car)
       (read-from-string)
       (apply #'values <> ,results))))

(defun test-radius ()
  (let ((pln (make-instance 'sizeless-polynomial)))
    (-<>>
	(loop for i from 0.5 downto 0.01 by 0.005
	       for new-poly = (md:modf (approx-step pln) i)
	      collect  (list i (total-run-time (radius new-poly 'r1 'r2))))
      (loop for i in <> do
	(format t "~&~{~a ~}" i)))))

(defmethod radius-slow ((this polynomial) x y)
  (-<>>
      (mapcar (lambda (var-x)
		   (loop for var-y in (axis-range this y) ;; <- collecting unstables
			 for current-poly = (vary this x y var-x var-y)
			 unless (stable-p current-poly)
			   collect (distance this current-poly x y)
			     into unstable-dists
			   finally (return unstable-dists)))
		  (axis-range this x))
    (remove-if-not #'identity)
    (al:flatten)
    (if <>
	(apply #'min <>)
	(all-stable-distance this x y))))


(defun test-radius-slow ()
  (let ((pln (make-instance 'sizeless-polynomial)))
    (-<>>
	(loop for i from 0.5 downto 0.01 by 0.005
	       for new-poly = (md:modf (approx-step pln) i)
	      collect  (list i (total-run-time (radius-slow new-poly 'r1 'r2))))
      (loop for i in <> do
	(format t "~&~{~a ~}" i)))))

(defgeneric radius! (this x y)
  (:documentation "bad version of radius"))

(defmethod radius! ((this polynomial) x y)
  (-<>>
      (lp:pmapcar (lambda (var-x)
		   (loop for var-y in (axis-range this y) ;; <- collecting unstables
			 for current-poly = (vary this x y var-x var-y)
			 unless (stable-p current-poly)
			   collect (distance this current-poly x y)
			     into unstable-dists
			   finally (return unstable-dists)))
		  (axis-range this x))
    (al:flatten)
    (remove-if-not #'identity)
    (if <>
	(apply #'min <>)
	(all-stable-distance this x y))))
