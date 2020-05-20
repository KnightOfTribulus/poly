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

(defmethod rstart ((this polynomial-piece)))

(defmethod rnominal ((this polynomial-piece)))

(defmethod rend ((this polynomial-piece)))

(defmethod initialize-instance ((this polynomial-piece) &key)
  (with-slots (label start nominal end) this
    (setf label (make-instance 'label :width 6 :master this))
    (pack label)
    (setf nominal (make-instance 'input-field :width 6 :height 1 :master this))
    (setf start   (make-instance 'input-field :width 6 :height 1 :master this))
    (setf end     (make-instance 'input-field :width 6 :height 1 :master this))))

(defclass polynomial-labels (data-frame)
  ((emp)
   (start)
   (nominal)
   (end)))

(defmethod initialize-instance :after ((this polynomial-labels) &key)
  (with-slots (emp start nominal end) this
    (setf emp (make-instance 'label :master this :text "" :width 6))
    (setf start (make-instance 'label :master this :text "от" :width 6))
    (setf nominal (make-instance 'label :master this :text "ном." :width 6))
    (setf end (make-instance 'label :master this :text "до" :width 6))
    (pack emp     :side :left)
    (pack start   :side :left)
    (pack nominal :side :left)
    (pack end     :side :left)))

(defclass shimmy-container (data-frame)
  ((label-container)
   (r1)
   (r2)
   (r3)
   (r4)
   (r5)
   (r6)
   (r7)
   (r8)
   (r9)
   (r10)
   (r11)
   (r12)
   (r13)
   (r14)
   (r15)
   (r16)
   (r17)
   (r18)))

(defmethod initialize-instance ((this shimmy-container) &key)
  (with-slots (label-container r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18) this
    (setf label-container (make-instance 'polynomial-labels))
    (setf r1   (make-instance 'polynomial-piece :init-side :top))   
    (setf r2   (make-instance 'polynomial-piece :init-side :top))   
    (setf r3   (make-instance 'polynomial-piece :init-side :top))   
    (setf r4   (make-instance 'polynomial-piece :init-side :top))   
    (setf r5   (make-instance 'polynomial-piece :init-side :top))   
    (setf r6   (make-instance 'polynomial-piece :init-side :top))   
    (setf r7   (make-instance 'polynomial-piece :init-side :top))   
    (setf r8   (make-instance 'polynomial-piece :init-side :top))   
    (setf r9   (make-instance 'polynomial-piece :init-side :top))   
    (setf r10  (make-instance 'polynomial-piece :init-side :top))  
    (setf r11  (make-instance 'polynomial-piece :init-side :top))  
    (setf r12  (make-instance 'polynomial-piece :init-side :top))  
    (setf r13  (make-instance 'polynomial-piece :init-side :top))  
    (setf r14  (make-instance 'polynomial-piece :init-side :top))  
    (setf r15  (make-instance 'polynomial-piece :init-side :top))  
    (setf r16  (make-instance 'polynomial-piece :init-side :top))  
    (setf r17  (make-instance 'polynomial-piece :init-side :top))  
    (setf r18  (make-instance 'polynomial-piece :init-side :top))))

(defun main ()
  (with-ltk ()
    (let* ((main-frame (make-instance 'data-frame))
	   (image (make-instance 'image-view))
	   (shim (make-instance 'shimmy-container :master main-frame)))
      )))

