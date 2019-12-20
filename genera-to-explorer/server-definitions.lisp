;;; -*- Mode:Common-Lisp; Package:NET; Base:10 -*-

;;; **********************************************************************
;;; Copyright (c) 1990 Stanford University.
;;; This code was written by members of the Large Knowledge Base project.
;;; Copyright is held by Stanford University except where code has been
;;; modified from TI source code.  In these cases TI code is marked with
;;; a suitable comment.  Where functionality implemented herein replicates
;;; similarly named functionality on Symbolics machines, this code was
;;; developed solely from the interface specification in the documentation
;;; or through guesswork, never by examination of Symbolics source code.

;;; All Stanford Copyright code is in the public domain.  This code may be
;;; distributed and used without restriction as long as this copyright
;;; notice is included and no fee is charged.  This can be thought of as
;;; being equivalent to the Free Software Foundation's Copyleft policy.

;;; TI source code may only be distributed to users who hold valid TI
;;; software licenses.
;;; **********************************************************************

;;;Edited by RICE                  22 Dec 89  13:01
;;;Edited by rice                  10 Jan 90  14:09
(defun add-logical-contact-name-translation (service-name implementation port)
  (maybe-create-logical-contact-name (generic-contact-name service-name))
  (let ((contact (find-logical-contact-name (generic-contact-name service-name))))
       (if (not contact) (ferror nil "~S does not name a contact." service-name))
       (without-interrupts
	 (let ((old (logical-contact-name-translations contact)))
	      (setf (logical-contact-name-translations contact)
		    (cons (list implementation port) (remove (assoc implementation old :test #'eq) old :test #'eq))
	      )
	      (logical-contact-name-translations contact)
	 )
       )
  )
)

(defun generic-contact-name (service-name)
  (string-capitalize (format nil "Generic ~A" service-name) :spaces t)
)

;;;Edited by rice                  16 Jan 90  18:12
(defun chaos:add-contact-name-for-protocol (name)
  (add-logical-contact-name-translation name :chaos (format nil "~A" name))
)

(defun tcp:add-tcp-port-for-protocol (server-name port-number)
  (add-logical-contact-name-translation server-name :tcp port-number)
)


(defun maybe-create-logical-contact-name (name)
  (or (find-logical-contact-name name)
      (push (make-instance 'logical-contact-name :name name) *logical-contact-name-list*)
  )
)

;;;Edited by RICE                  16 Jan 90  12:57
;;;Edited by RICE                  16 Jan 90  13:02
;;;Edited by BUILD-CYC             17 Jan 90  14:45
(defmacro define-server
	  (name (&key medium (stream '(stream)) (host 'host) who-line) &body body)
  (ignore who-line)
  (eh:require-pdl-room 13824. 4096)
  (let ((server-function-name (intern (string-append (symbol-name name) "-SERVER") 'services))
        (stream-name (first stream))
	(generic-logical-contact-name (generic-contact-name name))
	(server-name (string-capitalize (format nil "~A server" name) :spaces t))
	(stream-type (case (getf (rest stream) :characters)
		       (nil :binary-stream)
		       (otherwise :ascii-translating-character-stream)
		     )
	)
       )
      `(progn (defun ,server-function-name (contact-name)
		(declare (sys:function-parent ,name))
		(let ((*logical-contact-name* contact-name))
		     (declare (special *logical-contact-name*))
		     (with-open-stream
		       (,stream-name (listen-for-connection-on-medium
					  ,medium ,generic-logical-contact-name
					  :stream-type ,stream-type
				     )
		       )
		       (ignore ,stream-name)
		       (let ((,host local-host))
			    (ignore ,host)
			    (locally ,@body)
		       )
		     )
	        )
	      )
	      (maybe-create-logical-contact-name ,generic-logical-contact-name)
	      (add-server-for-medium ,medium ,generic-logical-contact-name
				     '(process-run-function '(:name ,server-name) ',server-function-name
							    ,generic-logical-contact-name
				      )
              )
	      (define-service ,name nil nil ,(format nil "Implements the ~S service." name))
	      (sys:record-source-file-name ',name :server)
	      (eval-when (compile) (compiler:compilation-define ',name))
	      ',name
       )
  )
)


(export 'define-server 'net)

(import 'define-server 'neti)

;;;Edited by rice                  10 Jan 90  13:43
(defun invoke-service-on-host (service-name host &rest args)
  (let ((client-name (intern (symbol-name service-name) 'services)))
       (apply client-name host args)
  )
)

(defmethod protocol-body-generator
	   ((type (eql :invoke))
	    stream-name stream-type actual-body medium generic-logical-contact-name
	   )
  (ignore stream-name stream-type medium generic-logical-contact-name)
  `(,@actual-body)
)

;;;Edited by rice                  16 Jan 90  18:32
(defmethod protocol-body-generator
	   ((type (eql :invoke-with-stream-and-close))
	    stream-name stream-type actual-body medium generic-logical-contact-name
	   )
  `(with-open-stream
     (,stream-name (open-connection-on-medium (parse-host host)
			,medium ,generic-logical-contact-name
			,@stream-type
		   )
     )
     ,@actual-body
   )
)

(defmethod parse-protocol (service-name (type (eql :invoke-with-stream-and-close)) &rest body-exprs)
  (ignore service-name)
  (destructuring-bind (arglist . actual-body) body-exprs
    (values arglist actual-body)
  )
)

(defmethod parse-protocol (service-name (type (eql :invoke)) &rest body-exprs)
  (ignore service-name)
  (values nil (list (first body-exprs) 'service-access-path))
)

;;;Edited by rice                  9 Jan 90  18:31
(defun parse-connection-args (&key (characters t))
  (append nil ;;; Add extra arg parsing here.
	  (list :stream-type
		(case characters
		  (nil :binary-stream)
		  (otherwise :ascii-translating-character-stream)
		)
	  )
  )
)

;;;Edited by RICE                  18 Dec 89  15:35
(defmethod parse-protocol-stream ((type (eql :invoke-with-stream-and-close)) arglist)
  (if (consp (first arglist))
      (values (first (first arglist))
	      (apply 'parse-connection-args (rest (first arglist)))
      )
      (values (first arglist) nil)
  )
)

(defmethod parse-protocol-stream ((type (eql :invoke)) arglist)
  (ignore arglist)
  (values nil nil)
)

;;;Edited by RICE                  18 Dec 89  15:11
(defstruct (service-access-path :named)
  host
  medium
  contact-name
)

;;;Edited by RICE                  18 Dec 89  15:11
;;;Edited by rice                  9 Jan 90  17:58
(defmacro with-service-access-path ((host medium contact-name) path &body body)
 `(let ((.path. ,path))
       (let ((,host (service-access-path-host .path.))
	     (,medium (service-access-path-medium .path.))
	     (,contact-name (service-access-path-contact-name .path.))
	    )
	    ,@body
       )
  )
)

;;;Edited by RICE                  15 Jan 90  12:07
;;;Edited by RICE                  16 Jan 90  12:57
;;;Edited by rice                  16 Jan 90  18:32
(defmacro define-protocol (name (ignore medium) &body body)
  (let ((client-function-name (intern (string-append (symbol-name name)) 'services))
	(generic-logical-contact-name (string-capitalize (format nil "Generic ~A" name) :spaces t))
	(type (first (first body)))
       )
       (multiple-value-bind (arglist actual-body) (apply 'parse-protocol name (first body))
	 (multiple-value-bind (stream-name stream-type) (parse-protocol-stream type arglist)
	   `(progn (defun ,client-function-name (host ,@(rest arglist))
		     (declare (sys:function-parent ,name))
		     (let ((service-access-path
			     (make-service-access-path
			       :host host :medium ,medium :contact-name ,generic-logical-contact-name
			     )
			   )
			   (*logical-contact-name* ,generic-logical-contact-name)
			  )
		          (declare (special *logical-contact-name*))
		          (ignore service-access-path)
		         ,(protocol-body-generator
			    type stream-name (or stream-type '(:stream-type :binary-stream))
			    actual-body medium generic-logical-contact-name
			  )
		     )
		   )
		   (sys:record-source-file-name ',name :protocol)
		   (eval-when (compile) (compiler:compilation-define ',name))
		   ',name
	    )
	  )
       )
  )
)


(export 'define-protocol 'net)
(import 'define-protocol 'neti)

;;;Edited by RICE                  18 Dec 89  15:11
;;;Edited by rice                  9 Jan 90  17:58
;;;Edited by rice                  9 Jan 90  18:22
(defun get-connection-for-service (service-access-path &rest connection-args)
  (with-service-access-path (host medium contact-name) service-access-path
    (apply 'open-connection-on-medium (parse-host host)
      medium contact-name (apply 'parse-connection-args connection-args)
    )
  )
)


;;;Edited by RICE                  18 Dec 89  15:11
;;;Edited by rice                  9 Jan 90  18:43
;;;Edited by RICE                  15 Jan 90  11:56
(defun net:find-path-to-service-on-host (service host)
  (ignore service host)
)



;;;Edited by RICE                  15 Jan 90  11:56
;;;Edited by BUILD-CYC             17 Jan 90  14:58
(defun find-medium-from-stream (stream)
  (ignore stream)
  :byte-stream
)

;;;Edited by rice                  9 Jan 90  19:29
(defmethod find-stream-type-from-stream ((me ip:binary-stream))
  :binary-stream
)

;;;Edited by rice                  9 Jan 90  19:29
(defmethod find-stream-type-from-stream ((me ip:ascii-translating-character-stream))
  :ascii-translating-character-stream
)

(defvar *stream-to-auxiliary-stream-mappings* nil)

(defun compute-contact-id (application-id)
  (format nil "~A-CONTACT-ID" application-id)
)

;;;Edited by RICE                  15 Jan 90  12:31
(defun compute-port (generic-contact-name implementation)
  (maybe-create-logical-contact-name generic-contact-name)
  (let ((contact (find-logical-contact-name generic-contact-name)))
       (if (not contact) (ferror nil "~S does not name a contact." generic-contact-name))
       (let ((trans (logical-contact-name-translations contact)))
	    (second (assoc implementation trans))
       )
  )
)

;;;Edited by RICE                  15 Jan 90  12:45
;;;Edited by RICE                  15 Jan 90  12:49
;;;Edited by RICE                  15 Jan 90  17:17
(defmethod compute-port-id ((stream ip:basic-stream))
  (+ 512 (min (send (send stream :connection) :destination-port)
	      (send (send stream :connection) :source-port)
	 )
  )
)

;;;Edited by RICE                  15 Jan 90  12:45
;;;Edited by RICE                  15 Jan 90  12:49
(defmethod compute-port-id ((stream chaos:basic-stream))
  nil
)


;;;Edited by RICE                  15 Jan 90  12:31
;;;Edited by RICE                  15 Jan 90  12:56
(defflavor aux-stream
	   ((actual-stream nil)
	    (connected-p nil)
	    stream-type
	    medium
	    contact-name
	    active-p
	    stream-options
	    starting-stream
	    host
	   )
	   (sys:input-stream sys:output-stream)
  :Gettable-instance-variables
  :Initable-instance-variables
  :settable-instance-variables
)

;;;Edited by RICE                  15 Jan 90  12:31
;;;Edited by RICE                  15 Jan 90  12:32
(defmethod (aux-stream :tyi) (&rest args)
  (lexpr-send actual-stream :tyi args)
)

;;;Edited by RICE                  15 Jan 90  12:31
;;;Edited by RICE                  15 Jan 90  12:32
(defmethod (aux-stream :untyi) (&rest args)
  (lexpr-send actual-stream :untyi args)
)

;;;Edited by RICE                  15 Jan 90  12:31
(defmethod (aux-stream :tyo) (&rest args)
  (lexpr-send actual-stream :tyo args)
)

;;;Edited by RICE                  15 Jan 90  12:31
;;;Edited by RICE                  15 Jan 90  17:17
(defmethod (aux-stream :close) (&rest args)
  (prog1 (if actual-stream (lexpr-send actual-stream :close args))
	 (setq actual-stream nil)
  )
)

;;;Edited by RICE                  15 Jan 90  17:04
;;;Edited by RICE                  15 Jan 90  17:17
;;;Edited by RICE                  16 Jan 90  11:50
(defun start-up-aux-stream-on-server (contact-id medium stream-type stream-options)
  (let ((stream (apply 'listen-for-connection-on-medium medium (generic-contact-name contact-id)
		       :stream-type stream-type stream-options
		)
	)
       )
       (loop for aux in *stream-to-auxiliary-stream-mappings*
	     when (and (equal contact-id (send aux :contact-name))
		       (not (send aux :active-p))
		  )
	     do (send aux :set-actual-stream stream)
	        (return aux)
	     finally (ferror nil "Not listening for contact id ~S" contact-id)
       )
       (send current-process :kill)
  )
)

;;;Edited by RICE                  16 Jan 90  11:36
;;;Edited by RICE                  16 Jan 90  11:50
;;;Edited by BUILD-CYC             17 Jan 90  14:19
(defmethod (aux-stream :complete-connection) (&key (timeout (* 60 6)))
  (with-timeout (timeout nil)
    (if active-p
        (loop do
	      (condition-case ()
		(setq actual-stream
		      (apply 'open-connection-on-medium host medium (generic-contact-name contact-name)
			     :stream-type stream-type stream-options
		      )
		)
		(gni-medium-error nil)
	      )
	      until (typep actual-stream 'sys:stream)
	      do (net:reset t)
	         (sleep 1 "Pause until make connection.")
	)
	(process-wait "Aux server wait." #'(lambda (me) (send me :actual-stream)) self)
    )
  )
  (check-type actual-stream sys:stream)
  (with-timeout (timeout (ferror nil "Complete-connection timed out on ~S" self))
    (if active-p
	(progn (print contact-name actual-stream)
	       (force-output actual-stream)
	       ;;; Wait for handshake.
	       (loop for result = (read actual-stream)
		     until (equal contact-name result)
	       )
	)
	(progn (loop for result = (read actual-stream)
		     until (equal contact-name result)
	       )
	       (print contact-name actual-stream)
	       (force-output actual-stream)
	)
    )
  )
  (setq connected-p t)
)

;;;Edited by rice                  9 Jan 90  18:43
;(defmedium-method :accept ()
;  nil
;)


;;;Edited by rice                  9 Jan 90  18:43
;;;Edited by rice                  9 Jan 90  19:23
;;;Edited by RICE                  15 Jan 90  11:56
;(defmedium-method :reject (reason &rest format-args)
;  (ferror nil reason format-args)
;)


;;;Edited by RICE                  15 Jan 90  16:42
(defmethod host-at-other-end-of ((stream chaos:basic-stream))
  (net:get-host-from-address (chaos:foreign-address (send stream :connection)) :chaos)
)

;;;Edited by RICE                  15 Jan 90  16:42
;;;Edited by RICE                  15 Jan 90  17:00
(defmethod host-at-other-end-of ((stream ip:basic-stream))
  (net:get-host-from-address (send (send stream :connection) :source-address) :ip)
)

;;;Edited by RICE                  16 Jan 90  11:26
;;;Edited by RICE                  16 Jan 90  11:36
;;;Edited by RICE                  16 Jan 90  11:50
(defmedium-method :start-open-auxiliary-stream
	   (active-p &key (local-id nil) (foreign-id nil)
	    (stream-options nil) (application-id nil)
	   )
  (let ((stream-id (intern-as-keyword (if active-p foreign-id (compute-contact-id (or local-id application-id)))))
	(port (compute-port-id self))
	(medium (find-medium-from-stream self))
	(stream-type (find-stream-type-from-stream self))
       )
       (chaos:add-contact-name-for-protocol stream-id)
       (tcp:add-tcp-port-for-protocol stream-id port)
       (add-server-for-medium medium (generic-contact-name stream-id)
			     `(process-run-function '(:name ,(format nil "~A Server" stream-id))
						    'start-up-aux-stream-on-server
						    ',stream-id
						    ',medium ',stream-type ',stream-options
			      )
       )
       (if (not active-p)
	   (progn (eh:require-pdl-room 13824. 4096)
		  (compiler:compile-form
		    `(define-service ,stream-id nil nil
		       ,(format nil "Implements the auxilliary ~S stream service." stream-id)
		     )
		  )
	   )
       )
       (let ((stream (make-instance 'aux-stream :stream-type stream-type :medium medium :contact-name stream-id
				    :active-p active-p :stream-options stream-options :starting-stream self
				    :host (host-at-other-end-of self)
		     )
	     )
	    )
	    (push stream *stream-to-auxiliary-stream-mappings*)
	    (values stream stream-id)
       )
  )
)


(defmedium-method (:after :close) (&rest args)
  (loop for entry in *stream-to-auxiliary-stream-mappings*
	when (equal (send entry :starting-stream) self)
	do (lexpr-send entry :close args)
	   (setq *stream-to-auxiliary-stream-mappings*
		 (remove entry *stream-to-auxiliary-stream-mappings*)
	   )
  )
)


;-------------------------------------------------------------------------------


(defmethod (host :check-validity) (&rest ignore)
  t
)