;;;; lispfurcproxy.asd

(asdf:defsystem #:lispfurcproxy
  :description "Furcadia DS processor and proxy written in Common Lisp"
  :author "Michal \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :depends-on (#:usocket
	       #:flexi-streams
	       #:iterate
	       #:trivial-features
	       #:external-program
	       #:bordeaux-threads
	       #:trivial-garbage)
  :serial t
  :components ((:file "package")
	       (:file "config")
	       (:file "socket-reader")
               (:file "lispfurcproxy")))

