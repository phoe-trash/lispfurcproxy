;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LispFurcProxy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Copyright 2015, Michal "phoe" Herda.
;;;;
;;;; The whole project is licensed under GPLv3.
;;;;

(in-package #:lispfurcproxy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CONFIG FUNCTIONS

(defun make-config (&key
		      (local-port 65012)
		      (host "lightbringer.furcadia.com")
		      (port 22)
		      (times-until-gc 200))
  (list (cons :local-port local-port)
	(cons :host host)
	(cons :port port)
	(cons :times-until-gc times-until-gc)))
