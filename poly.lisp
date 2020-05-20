;;;; poly.lisp

(in-package #:poly)

;;;; GLOBALS:

;;;; UI UTILS:

(eval-when (:compile-toplevel)
  (defparameter *from*      "0")
  (defparameter *to*        "1")
  (defparameter *nominal* "0.5")
  (defparameter *cell-width* 6)
  (defparameter *cell-height* 1)
  (defun make-keyword (sym)
    (al:make-keyword (string-upcase (format nil "~a" sym))))
  (defun make-nth-name (sym n)
    (al:symbolicate sym (string-upcase (format nil "~a" n))))) 

(defmacro with-n-frames (n master-frame &body body)
  (list 'let*
	(loop for i from 1 to n
	      ;; define the rest 
	      collect `(,(make-nth-name 'frame- i)
			(make-instance 'frame ,@(when master-frame
						  (list :master master-frame))))
	      collect `(,(make-nth-name 'label- i)
			(make-instance 'label
				       :text ,(format nil "r~a" i)
				       :width ,*cell-width* 
				       :master ,(make-nth-name 'frame- i)))
	      
	      collect `(,(make-nth-name 'from- i)
			(make-instance 'text
				       :width ,*cell-width* :height ,*cell-height*
				       :master ,(make-nth-name 'frame- i)))
	      collect `(,(make-nth-name 'to- i)
			(make-instance 'text
				       :width ,*cell-width* :height ,*cell-height*
				       :master ,(make-nth-name 'frame- i)))
	      collect `(,(make-nth-name 'nominal- i)
			(make-instance 'text
				       :width ,*cell-width* :height ,*cell-height*
				       :master ,(make-nth-name 'frame- i))))
	(cons 'progn
	      (loop for i from 1 to n
		    collect `(setf (text ,(make-nth-name 'from- i))
				   ,*from*)
		    collect `(setf (text ,(make-nth-name 'to- i))
				   ,*to*)
		    collect `(setf (text ,(make-nth-name 'nominal- i))
				   ,*nominal*)
		    collect `(pack ,(make-nth-name 'frame- i))
		    collect `(pack ,(make-nth-name 'label- i) :side :left)
		    collect `(pack ,(make-nth-name 'from- i) :side :left)
		    collect `(pack ,(make-nth-name 'nominal- i) :side :left)		    
		    collect `(pack ,(make-nth-name 'to- i) :side :left)))
	`(progn ,@body)))

(defmacro polynomial-from-n-tb (n)
  (append '(make-instance 'sizeless-polynomial)
	  ;; reading bounds and nominals
	  `(:approx-step ,(read-from-tb 'approx-step-tb))
	  (loop for i from 1 to n
		append (list (make-keyword (symbolicate 'r i))
			     `(read-from-tb ,(symbolicate 'nominal- i)))
		append (list (make-keyword (symbolicate 'r i '-end))
			     `(read-from-tb ,(symbolicate 'to- i))) 
		append (list (make-keyword (symbolicate 'r i '-start))
			     `(read-from-tb ,(symbolicate 'from- i))))))

(defun make-plot ()
  "Builds a plot and loads image.")

(defun show-robust ()
  "Calculates robust radius and shows the result")

;;;; MAIN:
(defun main ()
  (with-ltk ()
    (let* ((master-frame (make-instance 'frame))
	   (approx-frame (make-instance 'frame))
	   (lab-frame (make-instance 'frame
				     :master master-frame))
	   (empty-label (make-instance 'label
				      :width *cell-width*
				      :master lab-frame))
	   (from-label (make-instance 'label
				      :text "от"
				      :width *cell-width*
				      :master lab-frame))
	   (nom-label (make-instance 'label
				     :text "ном."
				     :width *cell-width*
				     :master lab-frame))	  
	   (to-label (make-instance 'label
				    :text "до"
				    :width *cell-width*
				    :master lab-frame)))

      (pack master-frame)
      (pack lab-frame :side :top)
      (pack empty-label :side :left)
      (pack from-label :side :left)
      (pack nom-label :side :left)
      (pack to-label :side :left)
      (with-n-frames 18 master-frame
	))))
