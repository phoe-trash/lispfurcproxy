;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LispFurcProxy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Copyright 2015, Michal "phoe" Herda.
;;;;
;;;; The whole project is licensed under GPLv3.
;;;;

(in-package #:lispfurcproxy)

(defun value (key alist)
  "Given an alist and a key, returns a respective value or NIL if it's not found."
  (cdr (assoc key alist)))

(defun key (value alist)
  "Given an alist and a value, returns a respective key or NIL if it's not found."
  (car (rassoc value alist)))

(defun proxy ()
  (let ((init-state (initial-state)))
    (unwind-protect
	 (loop
	    for input = nil                    then (get-input (inputs state))
	    for state = init-state             then (program state input)
	    for output = (value :output state) do   (send output state)
	    until (quit-p state))
      (ignore-errors
	(close (value :client-stream init-state))
	(close (value :server-stream init-state))
	(socket-close (value :client-socket init-state))
	(socket-close (value :server-socket init-state))))))

(defun initial-state ()
  (let* ((host "lightbringer.furcadia.com")
	 (port 22)
	 (local-port 65012)
	 (timeout (/ (1- (expt 2 32)) 1000))
	 (furc-path "cd c:\\Program Files (x86)\\Furcadia\\ && Furcadia.exe")
	 (ext-fmt (make-external-format :latin-1 :eol-style :lf))
	 (times-until-gc 200)

	 (client-thread (make-thread (lambda () (uiop:run-program furc-path
								  :force-shell t
								  :ignore-error-status t))
				     :name "Furcadia.exe spawner"))
	 (client-socket (socket-listen "localhost" local-port :element-type '(unsigned-byte 8)
				       :reuse-address t))
	 (client-stream (make-flexi-stream (socket-stream
					    (socket-accept
					     (wait-for-input client-socket
							     :timeout timeout)))
					   :external-format ext-fmt
					   :element-type 'octet))
	 (server-socket (socket-connect host port :element-type '(unsigned-byte 8)))
	 (server-stream (make-flexi-stream (socket-stream server-socket)
					   :external-format ext-fmt
					   :element-type 'octet)))
    (list (cons :client-thread client-thread)
	  (cons :client-socket client-socket)
	  (cons :client-stream client-stream)
	  (cons :server-socket server-socket)
	  (cons :server-stream server-stream)
	  (cons :outputs nil)
	  (cons :times-until-gc times-until-gc)
	  (cons :gc-counter times-until-gc))))

(defun program (state input)
  (list (cons :client-thread (value :client-thread state))
	(cons :client-socket (value :client-socket state))
	(cons :client-stream (value :client-stream state))
	(cons :server-socket (value :server-socket state))
	(cons :server-stream (value :server-stream state))
	(cons :output (cond ((eq (car input) :client-stream)
			     (parse-client-speak (cdr input)))
			    ((eq (car input) :server-stream)
			     (parse-server-speak (cdr input)))))
	(cons :times-until-gc (value :times-until-gc state))
	(cons :gc-counter (cond
			    ((< (value :gc-counter state) 0)
			     (gc :full t)
			     (value :times-until-gc state))
			    (t
			     (1- (value :gc-counter state)))))))

(defun inputs (state)
  (loop
     (cond
       ((listen (value :client-stream state))
	(return (cons :client-stream (value :client-stream state))))
       ((and (listen (value :server-stream state)))
	(return (cons :server-stream (value :server-stream state))))
       (t
	(sleep 0.1)))))

(defun get-input (input)
  (let* ((keyword (car input))
	 (stream (cdr input)))
    (cons keyword (read-line stream))))

(defun parse-client-speak (string)
  (cons :server-stream string))

(defun parse-server-speak (string)
  (cons :client-stream string))

(defun output (state)
  (value :output state))

(defun output-to-repl (output)
  (format t "(~A) " (cond ((eq (car output) :client-stream) "SERVER >>(())")
			  ((eq (car output) :server-stream) "(())<< CLIENT")))
  (format t "~S~%" (cdr output)))

(defun send (output state)
  (let ((stream (value (car output) state))
	(string (cdr output)))
    (unless (null string)
      (output-to-repl (value :output state))
      (ignore-errors
	(format stream "~A" string)
	(terpri stream)
	(force-output stream)))))

(defun quit-p (state)
  (let ((output (value :output state)))
    (or (not (thread-alive-p (value :client-thread state)))
	(and (eq (car output) :server-stream)
	     (string= (cdr output) "quit"))
	(and (eq (car output) :client-stream)
	     (string= (cdr output) "]]")))))
