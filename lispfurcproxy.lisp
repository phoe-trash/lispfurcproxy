;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LispFurcProxy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Copyright 2015, Michal "phoe" Herda.
;;;;
;;;; The whole project is licensed under GPLv3.
;;;;

(in-package #:lispfurcproxy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HELPER FUNCTIONS

(defun value (key alist)
  (cdr (assoc key alist)))
(defun config-value (key state)
  (cdr (assoc key (value :config state))))
(defun key (value alist)
  (car (rassoc value alist)))
(defun get-unix-time ()
  (- (get-universal-time) 2208988800))

;; a tiny shortcutter for usocket features
(defmacro socket-accept-wait (&body body)
  `(socket-accept (wait-for-input ,@body)))
(defun make-server-socket (config)
  (socket-listen "127.0.0.1" (value :local-port config)
		 :element-type '(unsigned-byte 8)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MAIN FUNCTIONS

(let (server-socket quit-main)
  
  (defun main (&key (config (make-config)))
    (if server-socket (socket-close server-socket))
    (setf quit-main nil
	  server-socket (make-server-socket config))
    (unwind-protect
	 (iter (with x = 0) (until quit-main)
	       (for socket = (socket-accept-wait server-socket :timeout 1))
	       (when socket (make-thread (lambda () (proxy config socket))
					 :name (format nil "LispFurcProxy ~D (~D)"
						       (incf x) (get-unix-time)))))
      (kill server-socket)))

  (defun quit-main ()
    (setf quit-main t)))

(defun proxy (config accepted-socket)
  ()
  (let ((init-state (initial-state config accepted-socket)))
    (unwind-protect (loop until (quit-p state)
		       for input = nil then (input state) 
		       for state = init-state then (program state input)
		       do (output state))
      (kill-state init-state))))

(defun kill-state (state)
  (kill (value :server-reader state))
  (kill (value :client-reader state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STATE FUNCTIONS

(defun initial-state (config client-socket)
  (let* ((server-socket (socket-connect (value :host config)
					(value :port config)
					:element-type '(unsigned-byte 8)))
	 (client-reader (make-instance 'socket-reader :socket client-socket))
	 (server-reader (make-instance 'socket-reader :socket server-socket)))
    (list (cons :client-reader client-reader)
	  (cons :server-reader server-reader)
	  (cons :config config)
	  
	  (cons :output nil)
	  (cons :gc-counter (value :times-until-gc config)))))

(defun program (state input)
  (list (assoc :client-reader state)
	(assoc :server-reader state)
	(assoc :config state)
	
	(cons :output (program-output input))
	(cons :gc-counter (program-gc-counter state))))

(defun program-output (input)
  (cond ((eq (car input) :client)
	 (cons :client (parse-client-speak (cdr input))))
	((eq (car input) :server)
	 (cons :server (parse-server-speak (cdr input))))))

(defun program-gc-counter (state)
  (cond
    ((< (value :gc-counter state) 0)
     (gc :full t)
     (config-value :times-until-gc state))
    (t
     (1- (value :gc-counter state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; INPUT FUNCTIONS

(defun input (state)
  (unless (null state)
    (loop
       (cond
	 ((is-unread (value :server-reader state))
	  (return (cons :server (get-line (value :server-reader state)))))
	 ((is-unread (value :client-reader state))
	  (return (cons :client (get-line (value :client-reader state)))))
	 (t
	  (sleep 0.05))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PARSER FUNCTIONS

(defun parse-client-speak (string)
  string)

(defun parse-server-speak (string)
  string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; OUTPUT FUNCTIONS

(defun output-type (output)
  (cond ((eq (car output) :server) "SERVER >>(())")
	((eq (car output) :client) "(())<< CLIENT")))

(defun output-to-repl (output)
  (format t "(~A) " (output-type output))
  (format t "~S~%" (cdr output)))

(defun output (state)
  (let* ((output (value :output state))
	 (reader (case (car output)
		   (:client (value :server-reader state))
		   (:server (value :client-reader state))))
	 (string (cdr output)))
    (unless (or (null string)
		(string= "" string))
      (output-to-repl (value :output state))
      (send-line (format-charlist (string-charlist string)) reader))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; QUIT FUNCTIONS

(defun quit-p (state)
  (unless (null state)
    (or (is-dead (value :client-reader state))
	(is-dead (value :server-reader state)))))
