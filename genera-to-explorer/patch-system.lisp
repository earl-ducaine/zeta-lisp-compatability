;;; -*- Mode:Common-Lisp; Package:System; Base:10 -*-

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

(defvar *files-being-patched* nil)

(defmacro files-patched-in-this-patch-file (&body files)
  (setq *files-being-patched* files)
 `(progn (setq *files-being-patched* ',files) (values))
)

(defun begin-patch-section-1 ()
  (sys:turn-common-lisp-on)
  (setq *read-base* 10.)
  (setq *print-base* 10.)
  (setq *package* (find-package 'user))
  (values)
)

(defmacro begin-patch-section ()
  (begin-patch-section-1)
 `(begin-patch-section-1)
)

(defmacro patch-section-source-file (file-name)
  (declare (special compiler:patch-source-file))
  (setq compiler:patch-source-file file-name)
 `(setq compiler:patch-source-file ',file-name)
)

(defun fs:attribute-bindings-from-list (attlist pathname)
;;; Changed by JPR to allow later mode line entries to override earlier ones.
  (do ((attlist attlist (cddr attlist))
       (vars nil)
       (vals nil)
       (binding-function))
      ((null attlist)
       (values  vars  vals))
    (and (setq binding-function (get (car attlist) 'fs:file-attribute-bindings))
       (multiple-value-bind (vars1 vals1)
	 (funcall binding-function pathname (car attlist) (cadr attlist))
	 (setq vars (nconc vars vars1)  ;;; (nconc vars vars1) changed by JPR
	        ;;; (nconc vals vals1) changed by JPR.
	       vals (nconc vals vals1))))))


(defun (:property :syntax fs:file-attribute-bindings)
       (ignore ignore syntax-keyword)
  (declare (values lisp-mode-variables-list lisp-mode-values-list))
  (if (not (eq syntax-keyword :common-lisp)) (setq syntax-keyword :zetalisp))
  (values (list* '*lisp-mode*
		 '*readtable*
		 'si:*reader-symbol-substitutions*
		 'zwei:*default-major-mode*
		 nil)
	  (case syntax-keyword
	    (:common-lisp
	     (list* :common-lisp
		    si:common-lisp-readtable
		    si:*common-lisp-symbol-substitutions*
		    :common-lisp
		    nil))
	    (:Zetalisp (beep)
	     (list* :zetalisp
		    si:standard-readtable
		    si:*zetalisp-symbol-substitutions*
		    :zetalisp
		    nil)))))


(defun patch-section-attributes-1 (attribute-string)
  (declare (special compiler:patch-source-file))
  (multiple-value-bind (vars vals)
      (fs:attribute-bindings-from-list
	(fs:file-parse-property-list attribute-string)
	compiler:patch-source-file
      )
    (loop for var in vars
	  for val in vals
	  do (setf (symbol-value var) val)
    )
    (values vars vals)
  )
)

(defmacro patch-section-attributes (attribute-string)
  (patch-section-attributes-1 attribute-string)
 `(progn (patch-section-attributes-1 ',attribute-string) (values))
)


(defmacro with-patch-environment (&body body)
 `(with-common-lisp-on
    (with-standard-io-environment
      (let ((*files-being-patched* nil)
	    (compiler:patch-source-file nil)
	    (eh:*source-code-debugging-enabled* nil)
	   )
	   (declare (special compiler:patch-source-file))
	   ,@body
      )
    )
  )
)

(defun compile-genera-patch-file (&rest args)
  (with-patch-environment (apply 'compile-file args))
)

(defun load-genera-patch-file (&rest args)
  (with-patch-environment (apply 'load args))
)

(defun compile-load-if-genera-patch-file (&rest args)
  (with-patch-environment (apply 'compile-load-if args))
)

(defun compile-if-genera-patch-file (&rest args)
  (with-patch-environment (apply 'compile-if args))
)

(defun load-if-genera-patch-file (&rest args)
  (with-patch-environment (apply 'load-if args))
)

;;;Edited by RICE                  20 Feb 90  17:02
;;;Edited by rice                  21 Feb 90  15:47
(defun compile-or-load-file-p (file)
  (letf ((#'compile-file
	  #'(lambda (&rest ignore) (throw 'probe :Compile-and-load))
	 )
	 (#'load #'(lambda (&rest ignore) (throw 'probe :load)))
	)
        (let ((eh:*source-code-debugging-enabled* nil))
	     (catch 'probe (compile-load-if file))
	)
  )
)

;;;Edited by rice                  21 Feb 90  15:47
(defun probe-genera-patch-file (path &key (verbose t))
  (let ((string (send path :String-For-Printing)))
       (let ((result (Compile-Or-Load-File-P path)))
	    (case result
	      (:Compile-And-Load
	       (if verbose
		   (format t "~&~A needs to be compiled and loaded." string)
	       )
	       (values path result)
	      )
	      (:Load (if verbose (format t "~&~A needs to be loaded." string))
		     (values path result)
	      )
	      (otherwise nil)
	    )
       )
  )
)

;-------------------------------------------------------------------------------

