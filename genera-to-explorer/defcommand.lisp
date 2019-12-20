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

(defvar *add-these-delimiters*)

(advise read-delimited-string :Around :maybe-change-delimiters nil
  (if (and (boundp '*add-these-delimiters*)
	   *add-these-delimiters*
      )
      (setf (first arglist)
	    (union (first arglist) *add-these-delimiters* :Test #'char-equal)
      )
      nil
  )
  :Do-It
)

(defvar *scl-to-ticl-command-table-mappings*
	'(("Global" ucl:universal-commands)
	  ("User" ucl:universal-commands)
	 )
)


(defun map-to-ticl-command-table (name)
  (let ((entry (assoc (string name) *scl-to-ticl-command-table-mappings*
		      :test #'string-equal
	       )
	)
       )
       (if entry (second entry) name)
  )
)


(defmethod command-table-equal ((x symbol) comtab)
  (eq (map-to-ticl-command-table x) comtab)
)


(defmethod command-table-equal ((x list) comtab)
  (if (equal (first x) 'quote)
      (command-table-equal (second x) comtab)
      (ferror nil "~S is no the name of a command table." x)
  )
)


(defmethod command-table-equal ((x string) comtab)
  (eq (map-to-ticl-command-table x) comtab)
)


(defmethod command-table-equal ((x ucl:command-table) comtab)
  (eq (send x :symbol-name) comtab)
)



(defun find-command-table-in-window (window command-table)
  (or (and (typep window 'ucl:basic-command-loop)
	   (or (find command-table (send window :all-command-tables)
		     :test 'command-table-equal
	       )
	       (find command-table (send window :system-command-tables)
		     :test 'command-table-equal
	       )
	       (find command-table (send window :special-command-tables)
		     :test 'command-table-equal
	       )
	   )
      )
      (loop for inf in (send window :inferiors)
	    for result = (find-command-table-in-window inf command-table)
	    when result
	    return result
	    finally (return nil)
      )
  )
)
      

;;;Edited by RICE                  7 Jan 90  15:55
(defun ticl:find-command-table (comtab)
  (loop for screen in tv:all-the-screens
	for result = (find-command-table-in-window screen comtab)
	when result
	return result
	finally (let ((entry (assoc comtab *scl-to-ticl-command-table-mappings* :test #'string-equal)))
		     (if entry
			 (return (get-command-table-names (second entry)))
			 (ferror nil "There is no command table called ~S" comtab)
		     )
		)
  )
)

(export 'ticl:find-command-table 'ticl)

;;;Edited by RICE                  7 Jan 90  15:39
(defun good-package (package)
"Given a package, finds a good package in which to intern some new symbol.
This will be the MM package if the user is trying to intern into the lisp or
keyword packages.
"
  (if (member package (mapcar #'find-package '(keyword lisp)))
      (find-package 'sys)
      package
  )
)

;;;Edited by RICE                  7 Jan 90  15:55
(defun get-command-table-names (name)
  (let ((*name (intern (string-append "*" (symbol-name name) "-COMMAND-TABLE*")
		       (good-package (symbol-package name))
	       )
	)
	(*all-name (intern (string-append "*ALL-" (symbol-name name)
					  "-COMMAND-NAMES*"
			   )
			   (good-package (symbol-package name))
	           )
	)
	(all-name (intern (string-append "ALL-" (symbol-name name)
					  "-COMMAND-NAMES"
			  )
			  (good-package (symbol-package name))
	          )
	)
	(build-name (intern (string-append "BUILD-" (symbol-name name)
					  "-COMMAND-TABLE"
			  )
			  (good-package (symbol-package name))
	            )
	)
       )
       (values *name *all-name all-name build-name)
  )
)

;;;Edited by RICE                  7 Jan 90  15:39
;;;Edited by RICE                  7 Jan 90  15:55
(defmacro defcommand-table (name short-doc long-doc)
"Declares a new command table called Name.  Short-doc and Long-doc are
 doc strings given to the command table build code.
"
  (multiple-value-bind (*name *all-name all-name build-name) (get-command-table-names name)
    `(progn (defvar ,*name :Unbound
		     ,(string-append "The command table for "
				     (string-capitalize (symbol-name name))
		      )
	    )
	    (pushnew ',*name *all-command-tables*)
	    (si:function-spec-putprop ',*name ',name 'si:function-parent)
	    (defvar ,*all-name nil
		    ,(string-append
		       "All of the commands to be built into the "
		       (string-capitalize (symbol-name *name))
		       " command table for "
		       (string-capitalize (symbol-name name))
		     )
	    )
	    (si:function-spec-putprop ',*all-name ',name 'si:function-parent)
	    (defun ,all-name ()
	     ,(format nil "Returns all of the commands that are built into ~
			   the ~A command table for ~A"
		      (string-capitalize (symbol-name *name))
		      (string-capitalize (symbol-name name))
	      )
	      (declare (si:function-parent ,name))
	      ,*all-name
	    )
	    (defun ,build-name ()
	     ,(format nil "Builds the command table ~A for ~A"
		      (string-capitalize (symbol-name *name))
		      (string-capitalize (symbol-name name))
	      )
	      (declare (si:function-parent ,name))
	      (build-command-table ',*name 'mail-control-window
		(,all-name)
		:Init-Options
		'(:Name ,short-doc :Documentation ,long-doc)
	      )
	      ',name
	    )
	    (pushnew ',build-name *all-command-table-builders*)
	    (values ',name  ',*name ',*all-name ',all-name ',build-name)
     )
  )
)

(defvar *all-command-table-builders* nil)
(defvar *all-command-tables* nil)

;;;Edited by RICE                  7 Jan 90  15:39
;;;Edited by RICE                  7 Jan 90  15:55
(defun cp:make-command-table (text-name &rest ignore)
  (let ((name (intern (string-upcase (substitute #\- #\space text-name :test #'char-equal)) 'sys)))
       (multiple-value-bind (name *name *all-name all-name build-name) (eval `(defcommand-table ,name "" ""))
	 (ignore name *name *all-name all-name)
	 (funcall build-name)
       )
       (push (list text-name name) *scl-to-ticl-command-table-mappings*)
       (values text-name name)
  )
)


(export 'cp:make-command-table 'cp)

;;;Edited by RICE                  7 Jan 90  15:39
;;;Edited by RICE                  7 Jan 90  15:55
(defun cp:install-commands (comtab commands)
  (loop for command in commands do (add-command-to-command-table command comtab))
)

(export 'cp:install-commands 'cp)

;;;Edited by RICE                  7 Jan 90  15:39
(defun add-command-to-command-table (short-name comtab)
  (let ((command-table-name (ticl:find-command-table comtab)))
       (ucl:build-command-table command-table-name 'tv:window
	 (fs:cons-new short-name
		      (mapcar #'(lambda (x) (send x :defname))
			      (g-l-p
				(send (symbol-value command-table-name)
					   :Commands
				)
			      )
		      )
	 )
       )
  )
)


;;;Edited by rice                  10 Jan 90  15:47
(defun compute-arglist (arglist)
  (if arglist
      (let ((*all-define-command-values*
	      (mapcar #'(lambda (x)
			  (if (consp (first x)) ;; defaulted
			      (list (first (first x)) (second x))
			      (list (first x) nil)
			  )
			)
		      (set-difference arglist lambda-list-keywords)
	      )
	    )
	   )
	   (declare (special *all-define-command-values*))
	   (apply 'compute-arglist-1 arglist)
	   (mapcar #'second *all-define-command-values*)
      )
      nil
  )
)

(defun help-multiple-commands (stream)
  (declare (special *all-define-command-arguments*))
  (if (equal (length *all-define-command-arguments*) 1)
      (format stream "~&Legal argument is ~A.  "
	      (first (first *all-define-command-arguments*))
      )
      (format stream
	      "~&Legal arguments are ~{~A~^, ~} and ~A.  "
	      (mapcar #'first (butlast *all-define-command-arguments*))
	      (first (first (last *all-define-command-arguments*)))
      )
  )
)

;;;Edited by rice                  10 Jan 90  15:42
;;;Edited by rice                  10 Jan 90  15:47
(defun compute-arglist-1 (arg &rest args)
  (declare (special *all-define-command-values*))
  (progv (mapcar #'first *all-define-command-values*)
	 (mapcar #'second *all-define-command-values*)
    (case arg
      (nil nil)
      (&key
       (let ((*all-define-command-arguments*
	       (remove nil (mapcar 'build-command-argument
				   (set-difference args lambda-list-keywords)
			   )
	       )
	     )
	    )
	    (declare (special *all-define-command-arguments*))
	    (if *all-define-command-arguments*
		(letf (((symeval-in-instance (follow-syn-stream *standard-input*)
					     'ucl:typein-modes
			)
			'(define-command-arguments)
		       )
		      )
		      (format *query-io* "(Keywords) ")
		      (with-help-function (#'Help-Multiple-Commands)
			(loop for (tag result) =
			      (multiple-value-list
			       (catch :Newline-Found
				 (catch :Finished-Argument
				   (let ((*throw-on-newline* t))
					(declare (special *throw-on-newline*))
					(send *standard-input*
					      :Fetch-And-Execute
					)
				   )
				 )
			       )
			      )
			      until (equal tag :Newline-Found)
			      do nil
			)
		      )
		)
		nil
	    )
       )
      )
      (&optional (read-single-argument (first args))
		 (apply 'compute-arglist-1 (rest (rest args)))
      )
      (otherwise (read-single-argument arg)
		 (apply 'compute-arglist-1 args)
      )
    )
  )
)

;;;Edited by Gruber                1 Jan 90  16:49
;;;Edited by rice                  10 Jan 90  16:29
;;;Edited by BUILD-CYC             11 Jan 90  11:28
(defun read-single-argument (arg &optional (dont-built-p nil))
  (declare (special *all-define-command-values*))
  (destructuring-bind (name actual-arg)
		      (if dont-built-p arg (build-command-argument arg))
    (if name
	(destructuring-bind
	  (arg-name presentation-type &key documentation prompt
		    prompt-mode default mentioned-default when (name arg-name)
          )
	  actual-arg
	  (ignore arg-name name mentioned-default prompt-mode
		  documentation when default
		  prompt presentation-type
	  )
	  (format *query-io* " ")
	  (setf (second (assoc arg-name *all-define-command-values*))
;		(ticl:Prompt-And-Accept presentation-type prompt)
		(ticl:Accept presentation-type :stream *query-io*
			:default default :prompt prompt :prompt-mode (or prompt-mode :normal)
		)
	  )
	)
	nil
    )
  )
)

(defun build-command-argument (arg)
  (destructuring-bind
    (arg-name presentation-type &key documentation prompt
     prompt-mode default mentioned-default when (name arg-name)
    )
    (if (consp (first arg)) (first arg) arg) ;;; consp -> defaulted
    (ignore arg-name name mentioned-default prompt-mode
	    documentation when default prompt presentation-type
    )
    (if (or (not when) (eval when))
	(list (string name) (cons (first arg) (mapcar #'eval (rest arg))))
	nil
    )
  )
)

;-------------------------------------------------------------------------------

;;;Edited by rice                  10 Jan 90  16:29
(defmacro ticl:define-command (name-and-options arguments &body body)
  (if (symbolp name-and-options)
      (setq name-and-options (list name-and-options))
      nil
  )
  (destructuring-bind
    (short-name &key name command-table comtab explicit-arglist
		     provide-destination-keyword values
    )
    name-and-options
    (ignore values provide-destination-keyword)
    `(let ((command (ucl:defcommand ,short-name nil '(:description ,name)
		      (flet ((_body_function_
			       (,@(or explicit-arglist
				      (just-arg-names arguments)
				  )
			       )
			       ,@body
			     )
			    )
			    (apply #'_body_function_
				   (let ((*recursive-call* t))
				        (declare (special *recursive-call*))
					(compute-arglist ',arguments)
				   )
			    )
			    (signal 'abort "Start at the top.")
		      )
		    )
	   )
	  )
          ,@(if arguments `((setf (get command :immediate-p) t)) nil)
          (add-command-to-command-table
	    ',short-name ',(or comtab command-table)
	  )
	  command
     )
  )
)
(shadow 'ticl:define-command 'zwei) ;; There is already one of these in Zwei.
(export 'ticl:define-command 'ticl)
