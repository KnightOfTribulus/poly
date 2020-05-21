;;;; poly.lisp

(in-package #:poly)

;;;; INTERFACES:

(defgeneric read-double (this)
  (:documentation "Reads input from the object as a double float."))

(defgeneric read-data (this)
  (:documentation "Reads data from the container."))

(defgeneric handle-empty-input (this)
  (:documentation "Handles the condition, when the input is empty."))

(defgeneric display (place data)
  (:documentation "Displays data in the given place."))
;;;; IMPLEMENTATION:
(eval-when (:compile-toplevel)
  (defparameter *from*      "0")
  (defparameter *to*        "1")
  (defparameter *nominal* "0.5")
  (defparameter *cell-width* 6)
  (defparameter *cell-height* 1)

  (defun make-keyword (&rest syms)
    (al:make-keyword (string-upcase (format nil "~{~a~}" syms))))
  (defun make-nth-name (sym n)
    (al:symbolicate sym (string-upcase (format nil "~a" n)))))

(defmacro with-n-frames (n master-frame &body body)
  (list 'let*
	(loop for i from 1 to n
	      ;; define the rest 
	      collect `(,(symbolicate 'r- i '-frame)
			(make-instance 'frame ,@(when master-frame
						  (list :master master-frame))))
	      collect `(,(symbolicate 'r- i '-label)
			(make-instance 'label
				       :text ,(format nil "r~a" i)
				       :width ,*cell-width* 
				       :master ,(symbolicate 'r- i '-frame)))
	      
	      collect `(,(symbolicate 'r- i '-start)
			(make-instance 'text
				       :width ,*cell-width* :height ,*cell-height*
				       :master ,(symbolicate 'r- i '-frame)))
	      collect `(,(symbolicate 'r- i '-end)
			(make-instance 'text
				       :width ,*cell-width* :height ,*cell-height*
				       :master ,(symbolicate 'r- i '-frame)))
	      collect `(,(symbolicate 'r i)
			(make-instance 'text
				       :width ,*cell-width* :height ,*cell-height*
				       :master ,(symbolicate 'r- i '-frame))))
	(cons 'progn
	      (loop for i from 1 to n
		    collect `(setf (text ,(symbolicate 'r- i '-start))
				   ,*from*)
		    collect `(setf (text ,(symbolicate 'r- i '-end))
				   ,*to*)
		    collect `(setf (text ,(symbolicate 'r i))
				   ,*nominal*)
		    collect `(pack ,(symbolicate 'r- i '-frame))
		    collect `(pack ,(symbolicate 'r- i '-label) :side :left)
		    collect `(pack ,(symbolicate 'r- i '-start) :side :left)
		    collect `(pack ,(symbolicate 'r i)          :side :left)		    
		    collect `(pack ,(symbolicate 'r- i '-end) :side :left)))
	`(progn ,@body)))

(defmacro read-from-n-frames (n)
  (append  '(setf *current-poly*)
	   (list
	    (append '(make-instance 'sizeless-polynomial)
		      '(:approx-step (read-double approx-field))
		      (loop for i from 1 to n
			    collect (make-keyword 'r i)
			    collect `(read-double ,(symbolicate 'r i))
			    collect (make-keyword 'r i '-start)
			    collect `(read-double ,(symbolicate 'r- i '-start))
			    collect (make-keyword 'r i '-end)
			    collect `(read-double ,(symbolicate 'r- i '-end)))))))

(defmethod read-symbol ((this text))
  (handler-case
      (-<>>
	  (read-from-string (text this))
	(coerce <> 'symbol))
    (t (arg) (handle-empty-input arg))))

(defmethod read-double ((this text))
  (handler-case
      (-<>>
	  (read-from-string (text this))
	(coerce <> 'double-float))
    (t (arg) (handle-empty-input arg))))

(defmethod display ((this text) obj)
  (->>
      (format nil "~a" obj)
    (setf (text this))))

(defclass data-frame (frame)
  ((init-side :initform :left
	      :initarg :init-side)
   (init-border :initform :sunken
		:initarg :border-style)))

(defmethod initialize-instance :after ((this data-frame) &key)
  (with-slots (init-border init-side) this
    (configure this :relief init-border)
    (pack this :side init-side)))

(defclass image-view (canvas)
  ())

(defmethod initialize-instance :after ((this image-view)  &key)
  (configure this :width 640)
  (configure this :height 480)
  (pack this :side :left))

(defmethod display ((this image-view) (filename string))
  (let* ((img (make-image)))
    (image-load img filename)
    (create-image this 0 0 :image img)))

(defparameter *current-poly* nil)

(defun main ()
  (with-ltk ()
    (let* ((main-frame (make-instance 'data-frame))
	   (image (make-instance 'image-view :master main-frame))
	   (approx-frame (make-instance 'data-frame :master main-frame))
	   (approx-label (make-instance 'label :master approx-frame
					       :text "шаг"
					       :width *cell-width*))
	   (approx-field (make-instance 'text :master approx-frame
					:width *cell-width*
					      :height *cell-height*))
	   (axis-frame (make-instance 'data-frame :init-side :bottom
				      :master main-frame))
	   (axis-lab   (make-instance 'label :master axis-frame
					     :width *cell-width*
					     :text "Оси:"))
	   (axis-y     (make-instance 'text :master axis-frame
					    :width *cell-width*
					    :height *cell-height*))
	   (axis-x     (make-instance 'text :master axis-frame
					    :width *cell-width*
					    :height *cell-height*))
	   (output-frame (make-instance 'data-frame :master main-frame))
	   (output-lab   (make-instance 'label :master output-frame
					       :width *cell-width*
					       :text "=>"))
	   (output-field (make-instance 'text :master output-frame
					      :width *cell-width*
					      :height *cell-height*))) 

      (with-n-frames 18 main-frame
	(let ((rob-button (make-instance 'button :text "Робастн."
						 :master main-frame
						 :command (lambda ()
							    (read-from-n-frames 18)
							    (->> (robust-radius *current-poly*)
							      (display output-field)))))
	      (rad-button (make-instance 'button :text "Радиус"
						 :master main-frame
						 :command (lambda ()
							    (read-from-n-frames 18)
							    (->> (radius *current-poly*
									 (read-symbol axis-x)
									 (read-symbol axis-y))
							      (display output-field)))))
	      (plot-button (make-instance 'button :text "Построить"
						  :master main-frame
						  :command (lambda ()
							     (read-from-n-frames 18)
							     (print *current-poly*)
							     (->>
								 (plot-to-file *current-poly*
									       (read-symbol axis-x)
									       (read-symbol axis-y))
							       (display image))))))
	  (setf (text approx-field)  "0.05")
	  (pack approx-frame :side :bottom)
	  (pack approx-label :side :left)
	  (pack approx-field :side :left)
	  (pack axis-frame :side :bottom)
	  (pack axis-lab :side :left)
	  (pack axis-x :side :left)
	  (pack axis-y :side :left)
	  (pack output-frame :side :bottom)
	  (pack output-lab :side :left)
	  (pack output-field :side :left)
	  (pack rad-button :side :bottom)
	  (pack rob-button :side :bottom)
	  (pack plot-button :side :bottom)
	  (pack image :side :left))))))

