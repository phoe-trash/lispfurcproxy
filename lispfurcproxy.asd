;;;; lispfurcproxy.asd

(asdf:defsystem #:lispfurcproxy
  :description "Describe lispfurcproxy here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:usocket
	       #:flexi-streams
	       #:iterate
	       #:trivial-features
	       #:external-program
	       #:bordeaux-threads
	       #:trivial-garbage)
  :serial t
  :components ((:file "package")
               (:file "lispfurcproxy")))

