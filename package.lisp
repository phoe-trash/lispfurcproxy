;;;; package.lisp

(defpackage #:lispfurcproxy
  (:use #:cl
	#:bordeaux-threads
	#:usocket
	#:flexi-streams
	#:iterate
	#:external-program
	#:bordeaux-threads
	#:trivial-garbage))

