(ql:quickload '(arrow-macros trivia alexandria lparallel str poly))

(in-package poly)

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
