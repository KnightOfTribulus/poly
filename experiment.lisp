(ql:quickload '(arrow-macros trivia alexandria lparallel str poly modf))

(in-package :poly)

(defmacro real-time (&body thunk)
  (al:with-gensyms (outstr results)
    `(-<>>
	 (with-output-to-string (,outstr)
	   (let ((*trace-output* ,outstr))
	     (setq ,results (multiple-value-list (time ,@thunk)))
	     (terpri ,outstr)))
       (str:split ":")
       (cadr)
       (str:split "seconds")
       (car)
       (read-from-string)
       (apply #'values <> ,results))))

(defun test-radius ()
  (let ((pln (make-instance 'sizeless-polynomial)))
    (-<>>
	(loop for i from 0.5 downto 0.01 by 0.005
	       for new-poly = (md:modf (approx-step pln) i)
	      collect  (list (/ 1 (expt i 2))
			     (real-time (radius new-poly 'r1 'r2))))
      (loop for i in <> do
	(format t "~&~{~a ~}" i))))
  (fc:clear-cache-all-function-caches))

(defmethod radius-slow ((this polynomial) x y)
  (-<>>
      (mapcan (lambda (var-x)
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


(defun test-radius-slow ()
  (let ((pln (make-instance 'sizeless-polynomial)))
    (-<>>
	(loop for i from 0.5 downto 0.01 by 0.005
	       for new-poly = (md:modf (approx-step pln) i)
	      collect  (list (/ 1 (expt i 2))
			     (real-time (radius-slow new-poly 'r1 'r2))))
      (loop for i in <> do
	(format t "~&~{~a ~}" i))))
  (fc:clear-cache-all-function-caches))


(defun plot-speeds ()
  (with-plots (*standard-output* :debug t)
    (gp-setup :terminal '(pngcairo)
	      :output "experiment-1.png"
	      :key '(box lt -1 lw 2 opaque))
    (gp :set :xlabel "n")
    (gp :set :ylabel "τ(n)")
    (plot #'test-radius :with '(lines lw 5 linecolor rgb "green" title "параллельный алг."))
    (plot #'test-radius-slow :with '(lines lw 5 lt 3 linecolor rgb "red" title "последовательный алг."))))
