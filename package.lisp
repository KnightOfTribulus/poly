;;;; package.lisp

(defpackage #:poly
  (:use #:cl
	#:eazy-gnuplot
	#:arrow-macros 
	#:cmu-infix
	#:ltk)
  (:local-nicknames (#:md #:modf)
		    (#:fc #:function-cache)
		    (#:lp #:lparallel)
		    (#:al #:alexandria)
		    (#:tr #:trivia)))

(declaim (optimize (speed 3)))
