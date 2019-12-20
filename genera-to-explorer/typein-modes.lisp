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

(defvar *all-define-command-arguments*)

(defflavor define-command-arguments () (ucl:typein-mode)
  (:default-init-plist
   :auto-complete-p t
  )
)

(defmethod (define-command-arguments :complete-p) (syntax)
  (if (eq syntax :First-Atom)
      "Define Command Arguments"
      nil
  )
)

(defwhopper (define-command-arguments :complete) (&rest args)
  (declare (special *all-commands* *command-found* *commands-found*))
  (let ((results (multiple-value-list (lexpr-continue-whopper args))))
       (if (and (first results) user:auto-complete? (boundp '*all-commands*))
	   (progn (setq *commands-found*
			(union (first results) *commands-found*)
		  )
		  (setq *command-found* t)
		  (setq *all-commands*
			(union (first results) *all-commands*
			       :Test #'string-equal
			)
		  )
	   )
	   nil
       )
       (values-list results)
  )
)

;;;Edited by rice                  10 Jan 90  14:58
(defmethod (define-command-arguments :complete) (word type)
  (case type
    (:Recognition
     (loop for (command-name) in *All-Define-Command-Arguments*
	   For command in *All-Define-Command-Arguments*
	   When (string-equal word command-name
			      :End1 (length word) :End2 (length word)
	        )
	   collect command-name
     )
    )
    (:Apropos
     (loop for (command-name) in *all-define-command-arguments*
	   for command in *All-Define-Command-Arguments*
               when (search (the string (string word))
			    (the string (string command-name))
			    :test #'char-equal
		    )
               collect command-name
     )
    )
    (:Spelling-Corrected
     (let ((matches
	     (w:spell word (mapcar #'first *all-define-command-arguments*))
	   )
	  )
          (loop for match in matches
		collect match
	  )
     )
    )
  )
)

(Defmethod (define-command-arguments :handle-typein-p) (expression type)
  (cond ((and (member type '(tv:first-atom tv:atom tv:symbol) :test #'eq)
	      (symbolp expression)
	      ;;; Not defiend yet.
	      ;;; This hasn't been called fix it when we end up in the dbger.
	      (if-a-command-argument expression)
	 )
	 (values self ())
	)
	(t (values () (format nil "~s is not a defined argument" expression)))
  )
)

;;;Edited by rice                  10 Jan 90  14:58
;;;Edited by rice                  10 Jan 90  15:25
;;;Edited by RICE                  24 Jan 90  13:12
(defmethod (define-command-arguments :execute) (window)
  (declare (special ucl:key-sequence *all-define-command-values*))
  (ignore window)
  (let ((match (first (send self :complete
			    (make-array (length ucl:key-sequence)
					:element-type 'string-char
					:initial-contents ucl:key-sequence
			    )
			    :recognition
		      )
	       )
	)
       )
       (throw :Finished-Argument
	      (values :Finished-Argument
		      (second (assoc (second (assoc match *all-define-command-arguments* :test #'string-equal))
				     *all-define-command-values*
			      )
		      )
	      )
       )
  )
)

;;;Edited by rice                  10 Jan 90  14:58
(defun if-a-command-argument (expression)
  (assoc (string expression) *all-define-command-arguments* :test #'string-equal)
)

(defmethod (define-command-arguments :arglist) (expression)
  (if (and (symbolp expression) (if-a-command-argument expression))
      (format nil "Argument ~S" expression)
      (values nil (format nil "~S is not a defined argument" expression))
  )
)

(defparameter Define-Command-Arguments
	      (make-instance 'Define-Command-Arguments)
)