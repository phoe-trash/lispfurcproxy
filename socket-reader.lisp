;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LispFurcProxy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Copyright 2015, Michal "phoe" Herda.
;;;;
;;;; The whole project is licensed under GPLv3.
;;;;

(in-package :lispfurcproxy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SOCKET READER CLASS

(defclass socket-reader ()
  ((socket :initarg :socket
	   :initform (error "No :SOCKET argument provided."))
   stream
   (first-unread :accessor is-unread)
   thread
   lock))

(defgeneric is-dead (socket-reader))
(defgeneric kill (object))
(defgeneric get-line (socket-reader))
(defgeneric send-line (string socket-reader))

(defmethod initialize-instance :after ((reader socket-reader) &key)
  (with-slots (socket stream thread first-unread lock) reader
    (labels ((reader-loop ()
	       (iter (for (values line err) = (ignore-errors (read-line stream nil :eof)))
		     (until (or (typep err 'error)
				(eq line :eof)
				(is-dead reader)))
		     (with-lock-held (lock)
		       (setf first-unread (append first-unread (list line)))))))
      (setf first-unread nil
	    lock (make-lock)
	    stream (flex:make-flexi-stream (socket-stream socket)
					   :element-type '(unsigned-byte 8)
					   :external-format (flex:make-external-format
							     :iso-8859-1 :eol-style :lf))
	    thread (make-thread #'reader-loop
				:name "LispFurcProxy socket reader")))))

(defmethod is-dead ((reader socket-reader))
  (with-slots (thread) reader
    (not (thread-alive-p thread))))

(defmethod kill ((reader socket-reader))
  (with-slots (stream socket) reader
    (ignore-errors
      (close stream)
      (socket-close socket))))

(defmethod kill ((socket usocket))
  (ignore-errors
    (socket-close socket)))

(defmethod get-line ((reader socket-reader))
  (with-slots (first-unread lock) reader
    (with-lock-held (lock)
      (let ((line (car first-unread)))
	(setf first-unread (cdr first-unread))
	line))))

(defmethod send-line (string (reader socket-reader))
  (with-slots (stream) reader
    (ignore-errors
      (format stream "~A" string)
      (terpri stream)
      (force-output stream))))
