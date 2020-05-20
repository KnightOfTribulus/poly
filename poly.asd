;;;; poly.asd

(asdf:defsystem #:poly
  :description "An application written for graduatee work."
  :author "Alexander"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("trivia"
	       "eazy-gnuplot"
	       "modf"
	       "function-cache"
	       "arrow-macros"
	       "lparallel"
	       "alexandria"
	       "cmu-infix"
	       "ltk")
  :components ((:file "package")
	       (:file "sapa-package")
	       (:file "basic-math")
	       (:file "utils")
	       (:file "shimmy-coeffs")
               (:file "method-core")
	       (:file "shimmy-implementation")
	       ;;(:file "test") ;; <- this file doesn't exist
	       ))
