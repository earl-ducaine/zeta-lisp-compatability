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

(import 'tv:defun-rh 'si)

(defun install-new-completion-handler ()
  (setf (tv:complete-word-function tv:rh-completion-handler)
	'll-rh-complete-word
  )
)


(defwrapper (ucl:top-level-command-names :complete) ((word type) &body body)
;(defmethod (ucl:top-level-command-names :around :complete)
;	   (cont mt ignore word type)
 `(locally
    (declare (special *Mailer* *command-found* *commands-found* *all-commands*))
    (ignore word type)
  (Let ((results (multiple-value-list
;		   (funcall-with-mapping-table cont mt :Complete word type)
		   (progn ,@body)
		 )
	)
       )
       (if (and (first results) user:auto-complete? (boundp '*all-commands*))
	   ;;; Maybe go ahead and execute it.
	   (let ((command nil))
	        (ucl:Looping-Through-Command-Tables Nil
		  (loop for name in (first results) do
			(let ((com (send ucl:command-table :Lookup-Name name)))
			     (if (and com (get com :Immediate-P))
				 (progn (setq command com)
					(setq *commands-found*
					      (fs:cons-new com *commands-found*)
					)
				 )
				 nil
			     )
			     (setq *all-commands*
				   (fs:cons-new com *all-commands*)
			     )
			)
		  )
		)
		(if command
		    (setq *command-found* command)
		    nil
		)
		command
	   )
	   nil
       )
       (values-list results)
  )
 )
)

(defun-rh ll-rh-complete-word
    (type &OPTIONAL
	  (complete-function (tv:completion-function tv:rh-completion-handler))
	  no-error-p
    )
  (if (and (type-specifier-p 'w:live-listener)
	   (typep self 'w:live-listener-UCL-commands-mixin)
      )
      (ll-rh-complete-word-internal
	type complete-function no-error-p
      )
      (tv:rh-complete-word type complete-function no-error-p)
  )
)

(defresource rubout-handler-buffers ()
	     :Constructor (tv:make-rubout-handler-buffer)
	     :Initial-Copies 2
)

(defun maybe-do-recursive-command (the-result)
  (declare (special *command-found*))
  (if *command-found*
      (do-recursive-command *command-found* the-result)
      nil
  )
)

(defvar *newline-occurred*)

(defun ll-tyi-internal (result)
  (declare (special *throw-on-newline*))
  (if (and (not (listp result)) (char-equal result #\newline))
      (if (boundp '*throw-on-newline*)
	  (if *throw-on-newline*
	      
	      (throw :Newline-Found
		     (values :Newline-Found
			     (Ll-Rh-Complete-Word :recognition)
		     )
	      )
	      (progn (setq *newline-occurred* t) result)
	  )
	  result
      )
      result
  )
)

(defun no-help-available (alist stream)
  (ignore alist)
  (format stream "~&No help available.")
)

(advise tv:rh-com-basic-help :Around :present nil
  (locally (declare (special *help-function*))
    (if (boundp '*help-function*)
	(funcall *help-function* tv:selected-window)
	:Do-It
    )
  )
)

;;;Edited by RICE                  24 Jan 90  13:19
(defwhopper (ucl:basic-command-loop :handle-prompt) ()
  (declare (special *inhibit-handle-prompt*))
  (if (and (boundp '*inhibit-handle-prompt*)
	   *inhibit-handle-prompt*
      )
      nil
      (continue-whopper)
  )
)
  
;;;Edited by rice                  8 Jan 90  14:10
;;;Edited by RICE                  24 Jan 90  13:12
;;;Edited by RICE                  24 Jan 90  13:19
(defun completing-read (alist &key (help-function 'no-help-available))
"Alist is of the form '((\"name1\" value1) (\"name2\" value2)...)."
  (ignore help-function)
  (let ((*all-define-command-arguments*
	  (loop for (name value) in alist
		for i from 1
		collect (list name i)
	  )
	)
	(*all-define-command-values*
	  (loop for (name value) in alist
		for i from 1
		collect (list i value)
	  )
	)
	(*inhibit-handle-prompt* t)
       )
       (declare (special *all-define-command-values*
			 *all-define-command-arguments*
			 *inhibit-handle-prompt*
		)
       )
       (With-Help-Function
	 (#'(lambda (stream) (funcall help-function alist stream)))
	 (if (typep (follow-syn-stream *standard-input*) 'ucl:basic-command-loop)
	     (letf (((symeval-in-instance (follow-syn-stream *standard-input*)
					  'ucl:typein-modes
		     )
		     '(define-command-arguments)
		    )
		   )
		   (multiple-value-bind (tag result)
		      (catch :Newline-Found
			(catch :Finished-Argument
			  (let ((*throw-on-newline* t))
			       (declare (special *throw-on-newline*))
			       (send *standard-input* :Fetch-And-Execute)
			  )
			)
		      )
		     (ignore tag)
		     (format *query-io* " ")
		     result
		   )
	     )
	     (loop for result = (string-trim '(#\space #\tab) (read-line *standard-input*))
		   for entry = (assoc result alist :test #'string-equal) 
	           until entry
		   do (beep) (format t "~S is not a legal response.  Please enter one of ~{~A~^, ~}"
				     result (mapcar #'first alist)
			     )
		   finally (return (second entry))
	     )
	 )
       )
  )
)
       
(defmethod do-recursive-command ((command string) the-result)
  (declare (special *all-define-command-arguments* *all-define-command-values*))
  (ignore the-result)
  (let ((*throw-on-newline* nil)
	(*newline-occurred* nil)
       )
       (declare (special *throw-on-newline*))
       (let ((result
	       (let ((entry (assoc command *all-define-command-arguments*)))
		    (if (consp (second entry)) ;; command
			(read-single-argument entry t)
			(second ;; alist entry
			  (assoc (second entry) *all-define-command-values*)
			)
		    )
	       )
	     )
	    )
	    (If *Newline-Occurred*
		(Throw :newline-found (values :Newline-Found result))
		(throw :Finished-Argument (values :Finished-Argument result))
	    )
       )
  )
)

(defmethod do-recursive-command ((command ucl:command) the-result)
  (With-New-Rhb (the-result)
    (send self :Set-Command-Entry command)
    (Send self :Execute-Command)
  )
)

(defun shortest-of (names best)
  (if names
      (if (< (length (first names)) (length best))
	  (shortest-of (rest names) (first names))
	  (shortest-of (rest names) best)
      )
      best
  )
)

(defmethod find-shortest-name ((command ucl:command))
  (let ((names (mapcar #'first (send command :names))))
       (if names
	   (Shortest-Of (rest names) (first names))
	   (values nil t)
       )
  )
)

;(defmethod Find-Shortest-Name ((command cons))
;  (ferror nil "?");(first command)
;)

(defmethod Find-Shortest-Name ((command string))
  (values command t)
)

(defmethod Find-Shortest-Name ((command t))
  (values command t)
)

(defmethod Ok-As-Command? ((Command t)) nil)

(defmethod Ok-As-Command? ((Command ucl:command)) t)

(defmethod Ok-As-Command? ((Command string)) t)

;(defmethod Ok-As-Command? ((command cons)) nil)

(defun find-shortest-command (commands best best-name)
  (if (and commands (Ok-As-Command? best))
      (multiple-value-bind (new-name failed-p)
	  (find-shortest-name (first commands))
        (if failed-p
	    (Find-Shortest-Command (rest commands) best best-name)
	    (if (or (not best-name) (< (length new-name) (length best-name)))
		(Find-Shortest-Command
		  (rest commands) (first commands) new-name
		)
		(Find-Shortest-Command (rest commands) best best-name)
	    )
	)
      )
      (if best-name best nil)
  )
)

(defun ll-rh-complete-word-internal (type complete-function no-error-p)
  "Find the word at the cursor position and do completion on it Returns NIL
 if no completion was done"
  (if user:auto-complete?
      (let ((*command-found* nil)
	    (*commands-found* nil)
	    (*all-commands* nil)
	   )
	   (declare (special *command-found* *commands-found*
			     *all-commands*
		    )
	   )
	   (multiple-value-bind (result completed-p completions-found)
	       (tv:rh-complete-word type complete-function no-error-p)
	     (ignore completions-found)
;	     (if (tv:mouse-y-or-n-p "?") (cl:break))
	     (if *command-found*
		 (setq *command-found*
		       (if completed-p
			   *command-found*
			   (Find-Shortest-Command
			     (rest *commands-found*)
			     (first *commands-found*)
			     (Find-Shortest-Name (first *commands-found*))
			   )
		       )
		 )
		 nil
	     )
	     (if (and *command-found*
		      (equal (length *commands-found*) 1)
		      (should-be-immediate *command-found*)
		 )
		 (Maybe-Do-Recursive-Command result)
	     )
	     (values result completed-p completions-found)
	   )
      )
      (let ((*all-commands* nil))
	   (declare (special *all-commands*))
	   (tv:rh-complete-word type complete-function no-error-p)
      )
  )
)

(defmethod should-be-immediate ((command ucl:command))
  (get command :Immediate-P)
)

(defmethod should-be-immediate ((command string))
  (declare (special *all-define-command-arguments*))
  (assoc command *all-define-command-arguments* :Test #'string-equal)
)

(defmethod should-be-immediate ((argument cons))
  (ferror nil "?") nil
)

(defmethod should-be-immediate ((thing t))
  nil
)

(install-new-completion-handler)
