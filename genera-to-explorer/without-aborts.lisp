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

(defvar *current-without-aborts*)


(defmacro sys:with-aborts-enabled ((&rest identifiers) &body body)
  `(let ((*current-without-aborts*
	   (if (boundp '*current-without-aborts*)
	       (set-difference *current-without-aborts* ',identifiers)
	       nil
	   )
	 )
	)
        ,@body
   )
)


;;;Edited by rice                  8 Jan 90  14:11
(defun eh:print-error-message (condition sg brief stream)
  (send condition :print-error-message (or sg sys:%current-stack-group) brief (or stream *error-output*))
)


(defun cli:follow-synonym-stream (stream)
  (sys:follow-syn-stream stream)
)


(defun eh:enter-debugger (condition &rest ignore)
  (let ((eh:*error-depth* (1+ eh:*error-depth*)))
       (declare (special eh:*error-depth*))
       (eh:invoke-debugger condition)
  )
)

(defun without-aborts-handler (condition &rest format-args)
  (ignore condition)
  (if (and (boundp '*current-without-aborts*)
	   *current-without-aborts*
      )
      (progn (format *error-output*
		     "~&The program cannot safely be aborted now.~%"
	     )
	     (if format-args (apply #'format *error-output* format-args))
	     (let ((response
		     (ticl:prompt-and-accept
		       '((ticl:alist-member
			   :alist
			   (("Abort" :value abort
			     :documentation "Abort anyway."
			    )
			    ("Skip" :value skip
			     :documentation "Ignore this abort."
			    )
			   )
			 )
			)
		       "~&Do you want to Skip or Abort? (press † for help)"
		     )
		   )
		  )
	          (case response
		    (abort nil)
		    (otherwise (push :go-ahead eh:*condition-proceed-types*)
			       (values :go-ahead nil)
		    )
		  )
	     )
      )
      nil
  )
)

(defmacro sys:without-aborts
	  ((&optional identifier reason &rest format-args) &body body)
  (let ((identifier (if (stringp identifier) nil identifier))
        (reason (if (or (stringp identifier) (stringp reason)) nil reason))
	(format-args (if (stringp identifier)
			 (cons identifier (cons reason format-args))
		         (if (stringp reason)
			     (cons reason format-args)
			     format-args
			 )
		     )
	)
       )
       (ignore reason)
      `(condition-bind
	 (((sys:abort) #'(lambda (condition &rest ignore)
			   (Without-Aborts-Handler condition ,@format-args)
			 )
	  )
	 )
	 (let ((*current-without-aborts*
		 ,(if identifier
		      `(cons ',identifier
			     (if (boundp '*current-without-aborts*)
				 *current-without-aborts*
				 nil
			     )
		       )
		       '(if (boundp '*current-without-aborts*)
			    *current-without-aborts*
			    '(t)
			)
		   )
	       )
	      )
	      ,@body
	 )
      )
  )
)

