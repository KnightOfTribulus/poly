(in-package :poly)
;;;; UTILITIES:

(defun  read-from-tb (name)
  (-<>>
      (text name)
    (read-from-string <>))) ;; NEED EXCEPTION HANDLING!!!

(defun distance-1d (x y)
  (abs (- x y)))

(defun symbolicate (&rest symbols)
  (intern (string-upcase (format nil "~{~a~}" symbols))
	  :poly))

(defun negative-p (num)
  (< num 0))

(defun bool-to-int (bool)
  "Converts a unified boolean to integer 1=t, 0=nil."
  (if bool 1 0))

(defun comb (m list fn)
  "Maps all combinations of lenght m from list using fn."
  (labels ((comb1 (l c m)
	     (when (>= (length l) m)
	       (if (zerop m) (return-from comb1 (funcall fn c)))
	       (comb1 (cdr l) c m)
	       (comb1 (cdr l) (cons (first l) c) (1- m)))))
    (comb1 list nil m)))

(defun pair-combinations (list)
  "Computes all pair combinations from this list"
  (let ((result nil))
    (comb 2 list (lambda (c) (push c result)))))

(defun trim-zeros (list)
  (if (= (car list) 0)
      (trim-zeros (cdr list))
      list))

(defun list-combinations (&rest lists)
  (if (endp lists)
      (list nil)
      (mapcan (lambda (inner-val)
                (mapcar (lambda (outer-val)
                          (cons outer-val
                                inner-val))
                        (car lists)))
              (apply #'list-combinations (cdr lists)))))

(defun list< (a b)
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((= (first a) (first b)) (list< (rest a) (rest b)))
        (t (< (first a) (first b))) ))


(defun sethash (table key val) ;; INLINE IT
  "Sets a hash in a given table."
  (setf (gethash key table) val))

(defun distance-2d (coords1 coords2) ;; INLINE IT
  "Computes the distance between 2 points of 2D space."
  (destructuring-bind (x1 y1) coords1
    (destructuring-bind (x2 y2) coords2
      (sqrt (+ (expt (- x1 x2) 2)
	       (expt (- y1 y2) 2))))))

(fc:defcached find-real-roots (coeffs-list)
  "Finds real-roots using a list of polynomial coeffs (starting from the highest degree). Signals an error if the method fails. Memoized."
  (assert coeffs-list nil "All coeffs are zeros!!!")
  (-<>>
      coeffs-list 
    (multiple-value-bind (succ zeros)
	(sapa:zeros-of-polynomial <> :maximum-number-of-iterations 25)
      (assert succ nil "Sapa couldn't find ANY roots!")
      zeros)
    (map 'list (lambda (root) (realpart root)))))
