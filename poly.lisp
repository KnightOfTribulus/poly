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

(defclass input-field (text)
  ((width  :initform 6
	   :initarg :width)
   (height :initform 1
	   :initarg :height)
   (init-side :initform :left
	      :initarg :init-side)
   (init-contents :initform ""
		  :initarg :init-contents
		  :accessor init-contents))
  (:documentation "Represents an input field."))


(defmethod read-double ((this input-field))
  (handler-case
      (-<>>
	  (read-from-string (text this))
	(coerce <> 'double-float))
    (t (arg) (handle-empty-input arg))))

(defmethod initialize-instance :after ((this input-field) &key)
  (with-slots (init-contents init-side) this
    (setf (text this) init-contents)
    (pack this :side init-side)))

(defclass output-field (text)
  ((init-side :initform :left
	      :initarg :init-side)
   (init-contents :initform ""
		  :initarg :init-contents
		  :accessor init-contents))
  (:documentation "Represents an input field."))

(defmethod initialize-instance :after ((this output-field) &key)
  (with-slots (init-contents init-side) this
    (setf (text this) init-contents)
    (configure this :height 1)
    (configure this :width  6)
    (pack this :side init-side)))

(defmethod display ((this output-field) obj)
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

(defclass polynomial-piece (data-frame)
  ((init-side :initform :top)
   (label)
   (nominal)
   (start)
   (end)))

(defmethod start ((this polynomial-piece)))

(defmethod nominal ((this polynomial-piece)))

(defmethod end ((this polynomial-piece)))

(defmethod initialize-instance ((this polynomial-piece) &key)
  (with-slots (label start nominal end) this
    (setf label (make-instance 'lambel :width 6 :master this))
    (pack label)
    (setf nominal (make-instance 'input-field :width 6 :height 1 :master this))
    (setf start   (make-instance 'input-field :width 6 :height 1 :master this))
    (setf end     (make-instance 'input-field :width 6 :height 1 :master this))))

(defclass shimmy-container (data-frame)
  ((r1 :initform  (make-instance 'polynomial-piece :init-side :top))   
   (r2 :initform  (make-instance 'polynomial-piece :init-side :top))   
   (r3 :initform  (make-instance 'polynomial-piece :init-side :top))   
   (r4 :initform  (make-instance 'polynomial-piece :init-side :top))   
   (r5 :initform  (make-instance 'polynomial-piece :init-side :top))   
   (r6 :initform  (make-instance 'polynomial-piece :init-side :top))   
   (r7 :initform  (make-instance 'polynomial-piece :init-side :top))   
   (r8 :initform  (make-instance 'polynomial-piece :init-side :top))   
   (r9 :initform  (make-instance 'polynomial-piece :init-side :top))   
   (r10 :initform (make-instance 'polynomial-piece :init-side :top))  
   (r11 :initform (make-instance 'polynomial-piece :init-side :top))  
   (r12 :initform (make-instance 'polynomial-piece :init-side :top))  
   (r13 :initform (make-instance 'polynomial-piece :init-side :top))  
   (r14 :initform (make-instance 'polynomial-piece :init-side :top))  
   (r15 :initform (make-instance 'polynomial-piece :init-side :top))  
   (r16 :initform (make-instance 'polynomial-piece :init-side :top))  
   (r17 :initform (make-instance 'polynomial-piece :init-side :top))  
   (r18 :initform (make-instance 'polynomial-piece :init-side :top))))

(defun main ()
  (with-ltk ()
    (let* ((main-frame (make-instance 'data-frame))
	   (input (make-instance 'input-field :master main-frame
					      :width 6
					      :height 1))
	   (image (make-instance 'image-view))
	   (output (make-instance 'output-field :master main-frame
						:width 6
						:height 1))
	   (butt (make-instance 'button :master main-frame :text "Построить"
					:command (lambda ()
						   (display image "plot_R1_from:0_to:1_R2_from:0_to:1_by:0.1d0.png")))))
      (pack butt))))

