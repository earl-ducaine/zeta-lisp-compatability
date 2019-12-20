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

(defmacro fs:with-automatic-login-to-sys-host (&body body)
 `(progn (fs:force-user-to-login :sys)
	 ,@body
  )
)


(defun zwei:qsend-msg (user@host message)
  (zwei:qsend user@host message)
)

(defun zwei::send-message-string (user@host message &key subject)
  (mail:submit-mail message
		    :to (list user@host)
		    :subject subject
  )
)

(proclaim '(declaration dbg:error-reporter))


(defvar si:*interactive-bindings* nil)

(defmacro sys:standard-value-let ((&rest vars-and-vals) &body body)
 `(let ((si:*interactive-bindings*
	  (append (list ,@(loop for item in vars-and-vals
				collect (if (consp item)
					   `(list ',(first item) ,(second item))
					   `(list ',item nil)
					)
			  )
		  )
		  si:*interactive-bindings*
	  )
	)
       )
       (declare (special si:*interactive-bindings*))
       (let (,@vars-and-vals)
	    ,@body
       )
  )
)


(defun scl:process-kill (process)
  (send process :kill)
)


(import 'cl:*print-readably* 'scl)



(defmacro sys:with-stack-array ((var width &rest make-array-keys) &body body)
  `(let ((,var (make-array ,width ,@make-array-keys)))
        ,@body
   )
)


(defun process:process-idle-time (process)
  (send process :idle-time)
)


(defstruct (mouse :named))

(defun sys:mouse-x (mouse)
  (ignore mouse)
  tv:mouse-x
)

(defun sys:mouse-y (mouse)
  (ignore mouse)
  tv:mouse-y
)

(defvar *mouse* (make-mouse))

(defun tv:sheet-mouse-offsets (sheet)
  (values (tv:sheet-x-offset sheet)
	  (tv:sheet-y-offset sheet)
	  *mouse*
  )
)


(defun tv:sheet-screen (sheet)
  (if (typep sheet 'tv:screen)
      sheet
      (tv:sheet-screen (tv:sheet-superior sheet))
  )
)


(defun tv:sheet-mouse (sheet)
  (ignore sheet)
  *mouse*
)

(defun ticl:maxf (place &rest values)
 `(setf place (max ,place ,@values))
)

(shadow 'ticl:maxf 'w)
(export 'ticl:maxf 'ticl)


(export 'sys:location-contents 'ticl)

(defmacro process:without-preemption (&body body)
  `(ticl:without-interrupts ,@body)
)


(defun tv:mouse-set-blinker-definition-internal
       (sheet type x y visibility operation &rest args)
  (ignore sheet)
  (apply #'tv:mouse-set-blinker-definition type x y visibility operation args)
)


(defvar dw::*mouse-blinker-characters* nil)


(defvar si:*user* fs:user-id)

(sys:forward-value-cell 'si:*user* 'fs:user-id)

;;; I don't think that this is worth the trouble.   JPR.  1/18/90.
;(defun compute-*user* ()
;  (setq sys:*user* (net:find-object-named :user fs:user-id nil))
;)

;(add-initialization "Recompute-*user*" '(compute-*user*) :login)

(defun tv:minutes-idle (&optional (process sys:current-process))
  (declare (values minutes-or-nil seconds))
  (let ((seconds (send process :idle-time)))
       (if seconds (floor seconds 60) seconds)
  )
)


(export 'net:object-not-found-in-search-list 'net)

(defun net:find-object-named (class name &optional (error-p t))
  (let ((result (name:find-object name nil (list class))))
       (if (and error-p (not result))
	   (ferror 'net:object-not-found-in-search-list
		   "Cannot find object ~S in class ~S"
		   name class
	   )
	   result
       )
  )
)


(defparameter net:*standard-services-enabled* nil)

(defparameter net:*all-services* nil)

(export 'net:*standard-services-enabled* 'net)

(defun sys:enable-services
       (&optional (services net:*standard-services-enabled*))
  (let ((services (case services
		    (:all net:*all-services*)
		    (otherwise services)
		  )
	)
       )
       (ignore services) (reminder "Fix up sys:enable-services some time.")
  )
)


(import 'sys:enable-services 'net)


(export 'ticl:self 'scl)


(defstruct (dw:display-object :named)
  redisplayer
)

(defmacro dw:redisplayer ((&optional (stream nil)) &body body)
 `(sys:make-display-object
    :Redisplayer
      #'(lambda (,(or stream 'stream)) ,@(if stream nil '((ignore stream)))
		,@body
	)
  )
)


(defun dw:do-redisplay
       (redisplay-piece &optional (stream *standard-output*) &rest ignore)
  (funcall (display-object-redisplayer redisplay-piece) stream)
)


(defmacro dw:with-output-as-presentation ((&rest ignore) &body body)
  (reminder
    "Fix up dw:with-output-as-presentation some time.  Used by ~{~A~^, ~}"
    '("cs:verify-transmitting-operation-queue")
  )
  `(progn ,@body)
)


(defun dw:redisplayable-format (&rest args)
  (apply #'format args)
)


(defun tv:sheet-console (sheet)
"A no-op for the Explorer."
  (ignore sheet)
  nil
)

(defvar tv:*slb-main-console* nil)

(defvar color:color-screen
	(if (tv:color-system-p tv:default-screen) tv:default-screen nil)
)

(defvar tv:*background-lisp-interactor-screen-fraction*
	(truncate (tv:sheet-height tv:default-screen) 3)
)

(advise compiler:process-pervasive-declarations :Around
	:allow-new-decl-processors nil
  (let ((processed (loop for decl in (first arglist)
			 when (get (first decl) 'declaration-processor)
			 collect decl
		   )
	)
       )
       (setf (first arglist) (set-difference (first arglist) processed))
       (loop for (decl-name . decl-args) in processed do
	     (apply (get decl-name 'declaration-processor)
		    (compiler:compiland-function-name
		      compiler:*current-compiland*
		    )
		    decl-args
	     )
       )
       :do-it
  )
)


(advise compiler:compile-driver :around :record-load-time-evals nil
  (locally (declare (special *at-top-level* *things-to-eval-at-load-time*))
    (let ((at-top-level (not (boundp '*at-top-level*))))
	 (let-if at-top-level
		 ((*things-to-eval-at-load-time* nil)
		  (*at-top-level* nil)
		 )
	      (let ((results :do-it))
		   (if (and at-top-level *things-to-eval-at-load-time*)
		       (progn (setf (first arglist)
				    `(prog1 ,(first arglist)
					    ,@*things-to-eval-at-load-time*
				     )
			      )
			      :do-it
		       )
		       (values-list results)
		   )
	      )
	 )
    )
  )
)

(define-declaration-processor zwei:indentation (name &rest indents)
  (let ((form
	  `(setf (get ',name 'zwei:lisp-indent-offset) ',(copy-tree indents))
	)
       )
       (at-load-time form)
  )
)



(defun tv:mouse-sheet (mouse)
  (ignore mouse)
  tv:default-screen
)


(deftype mouse-char ()
  `(satisfies mouse-char-p)
)


(defun sys:parse-error (&rest args)
  (apply #'sys:parse-ferror args)
)


(advise login :around :no-recursive nil
  (setq fs:user-id (string (first arglist)))
  (let ((*logging-in* fs:user-id))
       (declare (special *logging-in*))
       :do-it
       (setq fs:user-id (string (first arglist)))
  )
  (if (boundp '*logging-in*)
      fs:user-id
      t
  )
)


(advise fs:force-user-to-login :around :no-recursive nil
  (if (boundp '*logging-in*)
      (setq fs:user-id (locally (declare (special *logging-in*)) *logging-in*))
      :do-it
  )
)


(advise si:get-system-version :around :make-sure-we-have-a-number nil
  (let ((results (multiple-value-list :do-it)))
       (if (first results)
	   (values-list results)
	   (values 0 0 :released)
       )
  )
)

;-------------------------------------------------------------------------------

;;; Compiler style checker function spec handler.


(defun sub-property-function-spec-handler
       (function function-spec &optional arg1 arg2)
  (destructuring-bind (ignore symbol sub-function-name indicator) function-spec
    (if (not (and (= (length function-spec) 4) (symbolp symbol)))
	(unless (eq function 'validate-function-spec)
	  (invalid-function-spec function-spec))
	(CASE function
	  (validate-function-spec t)
	  (fdefine (let ((functions (get symbol indicator)))
		        (setf (get symbol indicator)
			      (cons (list sub-function-name arg1)
				    (remove (assoc sub-function-name functions)
					    functions)))))
	  ((fdefinition fdefinedp)
	   (second (assoc sub-function-name (get symbol indicator))))
	  (fdefinition-location
	    (locf (second (assoc sub-function-name (get symbol indicator)))))
	  (fundefine (let ((functions (get symbol indicator)))
		          (setf (get symbol indicator)
				(remove (assoc sub-function-name functions)
					functions))))
	  (dwimify
	   (and (symbolp indicator)
		(multiple-value-bind (new-sym dwim-p)
		    (catch 'dwimify-package
		      (map-over-lookalike-symbols
			(symbol-name indicator)
			#'(lambda (new-symbol spec original-spec dwimify-info)
			    (or (eq new-symbol (caddr spec))
				(dwimify-package-2
				  `(,(car spec) ,(cadr spec) ,(third spec)
				    ,new-symbol)
				  original-spec dwimify-info t)))
			function-spec arg1 arg2))
		  (and dwim-p new-sym))))
	  
	  (otherwise
	    (function-spec-default-handler
	      function function-spec arg1 arg2))))))

(setf (get :sub-property 'function-spec-handler)
      'Sub-Property-Function-Spec-Handler
)

(defun multi-style-checker (symbol form)
  (loop for (name function) in (get symbol 'compiler:multi-style-checker)
	do (funcall function form)
  )
)

(defun (:Property compiler:style-checker sys:function-spec-handler)
       (function function-spec &optional arg1 arg2)
  ;;; Note we discard the name of the function, just putting it on the
  ;;; plist.
  (let ((symbol (third function-spec)))
       (if (not (get symbol 'compiler:style-checker))
	   (setf (get symbol 'compiler:style-checker)
		 #'(lambda (form) (multi-style-checker symbol form))
	   )
	   nil
       )
       (funcall (get :Sub-Property 'sys:function-spec-handler) function
	       `(:Sub-Property ,(third function-spec) ,(second function-spec)
		 compiler:multi-style-checker
		)
	       arg1 arg2
       )
  )
)

(defun compiler-form-p (x)
  (and (consp x)
       (symbolp (first x))
       (equal (find-package 'compiler) (symbol-package (first x)))
  )
)

;;;Edited by James Rice            19 Apr 90  12:47
(defun compiler:constant-form-p (form)
"Is true if FORM is a constant in the current compilation environment."
  (and (compiler:trivial-form-p form)
       (or (not (symbolp form))
	   (get form 'compiler:system-constant)
	   (compiler:get-from-environment form 'compiler:system-constant)
	   (and (boundp 'compiler:allvars)
		(let ((value (compiler:var-init (compiler:lookup-var form))))
		     (and (consp value)
			  (equal 2 (length value))
			  (member  (first value)
				   '(compiler:fef-ini-comp-c
				     compiler:fef-ini-setq
				    )
			  )
			  (not (Compiler-Form-P (second value)))
			  (compiler:constant-form-p (second value))
		     )
		)
	   )
       )
  )
)

;-------------------------------------------------------------------------------


;;;Edited by James Rice            19 Apr 90  12:47
(defmacro dw::with-in-band-menu ((&rest ignore) &body body)
  `(progn ,@body)
)


;;;Edited by James Rice            19 Apr 90  12:47
(defmacro dw::with-accept-help ((&rest ignore) &body body)
  `(progn ,@body)
)


;;;Edited by James Rice            19 Apr 90  12:47
;;;Edited by BUILD-CYC             30 Apr 90  12:27
(defun dw::heapify-string (string) string)



;-------------------------------------------------------------------------------

(import 'zlc:string-length 'scl)


;-------------------------------------------------------------------------------


;;;Edited by RICE                  3 May 90  12:37
(defun font-canonical-name (font)
  (if (si:mx-p)
      (let ((name nil))
           (tv:do-local-symbols (f (find-package 'fonts) nil)
	      (if (and (boundp f)
		       (typep (symbol-value f) 'tv:font)
		       (not (equal f (tv:font-name font)))
		       (equal font (symbol-value f))
		  )
		  (setq name f)
		  nil
	      )
	   )
	   (if name name (tv:font-name font))
      )
      (tv:font-name font)
  )
)

;;;Edited by RICE                  3 May 90  12:37
(defun si:backtranslate-font (font)
   (let ((style (first (rassoc (list (font-canonical-name font)) ;;; JPR. 
			       g7c:*character-style-explorer-font-alist* :test #'equal))))
     (scl:make-character-style (first  style) (second style) (third  style))))



;-------------------------------------------------------------------------------

(fmakunbound 'dbg:bug-report-recipient-system)
(fmakunbound 'dbg:bug-report-description)
(fmakunbound 'dbg:document-proceed-type)
(fmakunbound 'dbg:initialize-special-commands)
(fmakunbound 'sys:proceed)
(fmakunbound 'dbg:proceed-type-p)
(fmakunbound 'dbg:proceed-types)
(fmakunbound 'dbg:report)
(fmakunbound 'dbg:report-string)
(fmakunbound 'dbg:special-command)

;-------------------------------------------------------------------------------

(pushnew '((:fix   :bold          :small)      fonts:tvfont)
	 g7c:*character-style-explorer-font-alist*
	 :Test #'equalp
)


g7c:
;;;Edited by James Rice            9 Nov 90  17:41
;;;Edited by James Rice            9 Nov 90  17:54
(DEFUN SYS:GET-FONT (device character-set style &optional (error-p t) inquiry-only)
   "simplified version of Symbolics function (DEVICE, CHARACTER-SET,  ERROR-P, and INQUIRY-ONLY are ignored)"
   (declare (values font-symbol)
	    (ignore device character-set error-p inquiry-only))
     (let ((font (second (assoc style *character-style-explorer-font-alist* :test 'character-style-eql))))
       (if (consp font)
	   (sys:get-font :ignore :ignore font)
	   (or font (tv:font-name (let ((font :Default)) (tv:coerce-font font tv:default-screen)))))))

