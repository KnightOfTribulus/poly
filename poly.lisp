;;;; poly.lisp

(in-package #:poly)

;;;; GLOBALS:
(defparameter *from*      "0")
(defparameter *to*        "1")
(defparameter *nominal* "0.5")
;;;; UI UTILS:

(defun make-nth-name (sym n)
  (al:symbolicate sym (string-upcase (format nil "~a" n))))

(defmacro with-n-frames (n master-frame &body body)
  (list 'let*
	(loop for i from 1 to n
	      collect `(,(make-nth-name 'frame- i)
			(make-instance 'frame ,@(when master-frame
						  (list :master master-frame))))
	      collect `(,(make-nth-name 'label- i)
			(make-instance 'label
				       :text ,(format nil "r~a~0,5T" i)
				       :master ,(make-nth-name 'frame- i)))
	      
	      collect `(,(make-nth-name 'from- i)
			(make-instance 'text
				       :height 1
				       :width 6
				       :master ,(make-nth-name 'frame- i)))
	      collect `(,(make-nth-name 'to- i)
			(make-instance 'text
				       :height 1
				       :width 6
				       :master ,(make-nth-name 'frame- i)))
	      collect `(,(make-nth-name 'nominal- i)
			(make-instance 'text
				       :height 1
				       :width 6
				       :master ,(make-nth-name 'frame- i))))
	(cons 'progn
	      (loop for i from 1 to n
		    collect `(setf (text ,(make-nth-name 'from- i))
				   ,*from*)
		    collect `(setf (text ,(make-nth-name 'to- i))
				   ,*to*)
		    collect `(setf (text ,(make-nth-name 'nominal- i))
				   ,*nominal*)
		    collect `(grid ,(make-nth-name 'label- i) :side :left)
		    collect `(grid ,(make-nth-name 'frame- i))
		    collect `(grid ,(make-nth-name 'from- i) :side :left)
		    collect `(grid ,(make-nth-name 'nominal- i) :side :left)		    
		    collect `(grid ,(make-nth-name 'to- i) :side :left)))
	`(progn ,@body)))

(defun make-plot ()
  "Builds a plot and loads image.")

(defun show-robust ()
  "Calculates robust radius and shows the result")

;;;; MAIN:
(defun main (&key (debug nil))
  (with-ltk ()
    ))
