;;; -*- Mode:Common-Lisp; Package:SYSTEM; Base:10 -*-

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

(defmacro with-help-function ((function) &body body)
 `(let ((*help-function* ,function))
       (declare (special *help-function*))
       ,@body
  )
)


(defmacro with-new-rhb ((the-result) &body body)
 `(using-resource (buf Rubout-Handler-Buffers)
    (send (follow-syn-stream *query-io*) :eval-inside-yourself
	  '(tv:rh-insert-string " " 0 nil t nil)
    )
    (let ((result
	    (letf (((symeval-in-instance
		      (follow-syn-stream *standard-input*)
		      'tv:rubout-handler-buffer
		    )
		    buf
		   )
		  )
		  ,@body
	    )
	  )
	 )
	 (if (equal result :Rubout)
	     (values ,the-result t t)
	     nil
	 )
    )
  )
)

;-------------------------------------------------------------------------------

(defvar *meta-presentation-arguments* nil)

(defmacro define-meta-presentation-argument (name value)
 `(progn (setq *Meta-Presentation-Arguments*
	       (remove (assoc ',name *meta-presentation-arguments*)
		       *meta-presentation-arguments*
	       )
	 )
	 (Push '(,name ,value) *Meta-Presentation-Arguments*)
	 ',name
  )
)

;-------------------------------------------------------------------------------

(defvar *all-timer-queues* nil)

(defmacro define-time-queue (name &optional (variable nil))
 `(let ((new (make-timer-queue :name ,name)))
       (push new *all-timer-queues*)
       ,@(if variable `((defparameter ,variable new)) nil)
       (ticl:process-run-function
	 (list :Name ,(format nil "~A timer" name))
	 'timer-queue-processing-function new
       )
  )
)

(defstruct (timer-queue-entry :named)
  name
  function
  args
  repeat
  time
  id
  queue
)

(defstruct (timer-queue :named)
  name
  next-start-time
  next-active-entry
  queue
)

;;; This is now defined by 36xx-Explorer
;;;;Edited by RICE                  29 Dec 89  17:43
;(defmacro ticl:with-standard-io-environment (&body body)
; `(let ((*package* (find-package 'lisp))
;	(*readtable* sys:standard-readtable)
;	(*print-structure* t)
;	(*nopoint nil)
;	(*print-base* 10.)
;	(*read-base* 10.)
;       )
;       ,@body
;  )
;)

(export 'ticl:with-standard-io-environment 'ticl)


;;;Edited by RICE                  29 Dec 89  17:50
;;;Edited by rice                  5 Jan 90  16:05
;;;Edited by rice                  8 Jan 90  13:38
(defmacro zwei:with-interval-stream ((stream buffer) &body body)
 `(let ((.buffer. ,buffer))
       (with-open-stream (,stream (zwei:interval-stream (send .buffer. :first-bp) (send .buffer. :last-bp))) ,@body)
  )
)


(advise compiler:extract-declarations-record-macros :around :say-we-are-inside-extract... nil
  (let ((*inside-extract* t))
       (declare (special *inside-extract*))
       :do-it
  )
)

;;;Edited by rice                  5 Jan 90  16:05
;;;Edited by rice                  5 Jan 90  17:15
;;;Edited by rice                  8 Jan 90  13:38
(defmacro at-compile-time (&body forms)
  (let ((name (gensym "AT-COMPILE-TIME-")))
       (gensym "G")
      `(macrolet ((,name ()
		   (if (or (not (boundp 'sys:*inside-extract*))
			   (not sys:*inside-extract*)
		       )
		       (progn ,@forms)
		       nil
		   )
		   nil
		  )
		 )
	 (,name)
       )
  )
)


(export 'at-compile-time 'sys)

;;;Edited by rice                  9 Jan 90  15:15
;;;Edited by rice                  9 Jan 90  15:19
;;;Edited by rice                  9 Jan 90  16:03
(defun at-load-time (&rest body)
  (declare (special *things-to-eval-at-load-time*))
  (if (boundp '*things-to-eval-at-load-time*)
      (pushnew `(eval-when (load) (progn ,@body)) *things-to-eval-at-load-time* :test #'equalp)
      nil
  )
  nil
)

(export 'at-load-time 'sys)

;;;Edited by rice                  5 Jan 90  16:05
;;;Edited by rice                  9 Jan 90  16:03
(defmacro reminder (control-string &rest format-args)
;  (at-load-time `(compiler:warn 'reminder :probable-error ,control-string ,@format-args))
  `(at-compile-time (compiler:warn 'reminder :probable-error ,control-string ,@format-args))
)

(export 'reminder 'sys)


;;;Edited by RICE                  7 Jan 90  15:16
;;;Edited by RICE                  7 Jan 90  16:11
(defmacro define-declaration-processor (name (&rest args) &body body)
 `(progn (proclaim '(declaration ,name))
	 (defun (:property ,name declaration-processor) (,@args) ,@body)
	 ',name
  )
)


;;;Edited by RICE                  7 Jan 90  16:11
;;;Edited by RICE                  7 Jan 90  16:40
(defmacro sys:with-table-locked ((hash-table) &body body)
  `(with-lock ((hash-table-lock ,hash-table)) ,@body)
)


;;;Edited by RICE                  7 Jan 90  16:40
;;;Edited by BUILD-CYC             17 Jan 90  14:19
(defmacro cp:define-command-accelerator (name &rest ignore)
 `(sys:reminder "Do something about cp:define-command-accelerator ~S" ',name)
)


;-------------------------------------------------------------------------------

(defvar net:*all-medium-basic-stream-flavors* '(chaos:basic-stream ip:basic-stream))

;;;Edited by BUILD-CYC             17 Jan 90  14:19
(defmacro net:defmedium-method (name args &body body)
  (let ((method-name (if (consp name) (first (last name)) name))
	(method-qualifiers (if (consp name) name (list name)))
       )
       (let ((internal-name (intern (string-append (symbol-name method-name) "-INTERNAL") 'net))
	     (short-args (sys:just-arg-names args))
	    )
	   `(progn (defun ,internal-name ,short-args (declare (sys:function-parent ,method-name)) ,@body)
		 ,@(loop for flav in net:*all-medium-basic-stream-flavors* collect
			`(defmethod (,flav ,@method-qualifiers) ,args (declare (sys:function-parent ,method-name))
			   (,internal-name ,@short-args)
			 )
		   )
	    )
       )
  )
)

;;;Edited by BUILD-CYC             17 Jan 90  14:59
;;; Edited by Tom Gruber           19 Jan 90  18:13
(defun Just-Arg-Names (args)
  (loop for arg in args
	when (not (member arg lambda-list-keywords))
	collect (ucl:first-if-list arg)
  )
)



;;; Edited by Tom Gruber           19 Jan 90  18:13
(defmacro ticl:with-input-editing-options ((&rest options) &body body)
  (ignore options)
 `(progn ,@body)
)


(export 'ticl:with-input-editing-options 'ticl)