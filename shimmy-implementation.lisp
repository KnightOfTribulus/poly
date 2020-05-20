;; actual.lisp
(in-package :poly)

(defclass sizeless-polynomial (polynomial)
  ((%koef-applied? :initform nil
		   :accessor koef-applied-p)
   (%koef :initform 2
	  :accessor koef)
   (%approx-step :accessor approx-step
		 :initform 0.1d0)
   ;; constant parameters, verified:1
   (cap_A :initform 40)
   (cap_B :initform 45)
   (cap_C :initform 10)
   (cap_DD :initform 45)
   (ωω :initform 55)
   (k  :initform 50)
   (cap_V :initform 60)
   (cap_S :initform 1300)
   (l :initform 1.33)
   (c :initform 0.28)
   (cap_N :initform 21000)
   (h :initform 5)
   (σ :initform -0.5)
   (a :initform 10)
   (b :initform 70)
   (ρ :initform 0.2)
   (α :initform 1.611)
   (β :initform 1.836)
   ;; variable parameters:
   (r1 :initform 0.5)  (r1-start :initform 0) (r1-end :initform 1)
   (r2 :initform 0.5)  (r2-start :initform 0) (r2-end :initform 1)
   (r3 :initform 0.5)  (r3-start :initform 0) (r3-end :initform 1)
   (r4 :initform 0.5)  (r4-start :initform 0) (r4-end :initform 1)
   (r5 :initform 0.5)  (r5-start :initform 0) (r5-end :initform 1)
   (r6 :initform 0.5)  (r6-start :initform 0) (r6-end :initform 1)
   (r7 :initform 0.5)  (r7-start :initform 0) (r7-end :initform 1)
   (r8 :initform 0.5)  (r8-start :initform 0) (r8-end :initform 1)
   (r9 :initform 0.5)  (r9-start :initform 0) (r9-end :initform 1)
   (r10 :initform 0.5) (r10-start :initform 0) (r10-end :initform 1)
   (r11 :initform 0.5) (r11-start :initform 0) (r11-end :initform 1)
   (r12 :initform 0.5) (r12-start :initform 0) (r12-end :initform 1)
   (r13 :initform 0.5) (r13-start :initform 0) (r13-end :initform 1)
   (r14 :initform 0.5) (r14-start :initform 0) (r14-end :initform 1)
   (r15 :initform 0.5) (r15-start :initform 0) (r15-end :initform 1)
   (r16 :initform 0.5) (r16-start :initform 0) (r16-end :initform 1)
   (r17 :initform 0.5) (r17-start :initform 0) (r17-end :initform 1)
   (r18 :initform 0.5) (r18-start :initform 0) (r18-end :initform 1)))

(defmethod apply-koef ((this sizeless-polynomial))
  "Multiplies every non-r parameter of the polynomial to %koef for whatever reason.
Modifies initial-object and returns it."
  (assert (not (koef-applied-p this))
	  nil "Attempt to apply koef, which was already applied!")
  (mapcar (lambda (slt)
	    (setf (slot-value this slt)
		  (* (koef this) 
		     (slot-value this slt))))
	  '(cap_A cap_B cap_C cap_DD ωω k cap_V cap_S l c cap_N h σ a b ρ α β))
  (setf (koef-applied-p this) t)
  this)

(defmethod initialize-instance :after ((this sizeless-polynomial) &key)
  (apply-koef this))

(defmethod start ((this sizeless-polynomial) axis)
  (slot-value this
	      (symbolicate axis '-start)))

(defmethod end ((this sizeless-polynomial) axis)
  (slot-value this
	      (symbolicate axis '-end)))

(defmethod axis-range ((this sizeless-polynomial) axis)
  (loop for i from (start this axis) to (end this axis) by (approx-step this)
	collect i))

(defmethod eval-coefs ((this sizeless-polynomial))
  ;;(declare (inline q0 q1 q2 q3 q4 q5 q6))
  (with-slots (cap_A cap_B cap_C cap_DD ωω k cap_V cap_S l c cap_N h σ a b ρ α β r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18)
      this
      (list (q0)
	    (q1)
	    (q2)
	    (q3)
	    (q4)
	    (q5)
	    (q6))))

(defmethod distance ((this sizeless-polynomial)
		     (that sizeless-polynomial)
		     x y)
  (distance-2d (list (slot-value this x)
		     (slot-value this y))
	       (list (slot-value that x)
		     (slot-value that y))))

(defmethod all-stable-distance ((this sizeless-polynomial) x y) ;; terribly wrong!
  (min (distance-1d (slot-value this (symbolicate x '-start))
		    (slot-value this x))
       (distance-1d (slot-value this (symbolicate x '-end))
		    (slot-value this x))
       (distance-1d (slot-value this (symbolicate y '-start))
		    (slot-value this y))
       (distance-1d (slot-value this (symbolicate y '-end))
		    (slot-value this y))))

(defmethod center ((this sizeless-polynomial) x y)
  (list (slot-value this x)
	(slot-value this y)))

(defmethod axis-pairs ((this sizeless-polynomial))
  (pair-combinations '(r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18)))

(defmethod stable-p ((this sizeless-polynomial))
  (with-slots (a b c) this
    (->>
      (eval-coefs this) 
      (trim-zeros)
      (find-real-roots)
      (every #'negative-p)))) ;; negative-p is borked

(defmethod vary ((this sizeless-polynomial) x y new-x new-y)
  (-<>> this
    (md:modf (slot-value <> x) new-x)
    (md:modf (slot-value <> y) new-y)))

(defparameter *actual-poly* (make-instance 'sizeless-polynomial))
