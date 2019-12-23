



;;; Copyright (C) 1989 Texas Instruments Incorporated. All rights reserved.
;;;
;;;  7/05/88 PHD - Initial version.
;;; 10/31/88 DNG - Moved to a separate file from the rest of the translator.
;;; 11/02/88 DNG - Use new function FLAVOR-METHOD-SPEC-P to ensure consistency with
;;;                DEFMETHOD.
;;; 11/19/88 DNG - Translate body of DEFMETHOD.  Fix query for generic name.
;;;		   Add translation for SEND.  Etc.
;;; 11/21/88 DNG - Improve handling of DEFFLAVOR options.
;;; 11/22/88 DNG - Improve DEFMETHOD handling.
;;; 11/23/88 DNG - Add handling for MULTIPLE-VALUE-SETQ on instance variables.
;;; 11/27/88 DNG - Add special handling for :AROUND methods.
;;; 11/28/88 DNG - Fix to process default values of method arguments.
;;;		   Add translation for DEFWRAPPER.
;;; 11/29/88 DNG - Add translation of DEFUN-METHOD.
;;; 12/01/88 DNG - Improve handling of :INIT methods.
;;; 12/06/88 DNG - Add translation of DEFWHOPPER.
;;; 12/09/88 DNG - Add support of :METHOD-COMBINATION option of DEFFLAVOR.
;;; 12/13/88 DNG - Fix argument order on (DEFMETHOD (SETF ...)...).
;;;  1/05/89 DNG - Smarter about when to use SETF for :SET-... messages.
;;;  1/07/89 DNG - Add special handling for (SEND x :SET key value).  Enhance
;;;		   DEFMETHOD translation to try to ensure arglist congruence.  Special
;;;		   handling for arguments of :PRINT-SELF methods.
;;;  1/14/89 DNG - Add special handling for :SEND-IF-HANDLES.
;;;  1/21/89 DNG - Add special handling for :OPERATION-HANDLED-P.
;;;  2/24/89 DNG - Handle :INCLUDED-FLAVORS option of DEFFLAVOR.
;;;
;;; Not yet handled:	(DECLARE (:SELF-FLAVOR ...))
;;;		  	FUNCALL-WITH-MAPPING-TABLE, LEXPR-FUNCALL-WITH-MAPPING-TABLE

;; The following expands to:
;; (defun (:property p-defgeneric-options transform) (form)
;;   (change (first form) 'defgeneric))
;;
;; Roughly the meaning of this is: whenever you would ordinarily
;; execute the p-defgeneric-options macro on a top level form, instead
;; use the defgeneric macro.
(defreplace p-defgeneric-options defgeneric)

;; Should be able to 'just use' CLOS make-instance version
;; need load-time test
;; (unless (eq 'cl:make-instance 'zl:make-instance)
;;   (defreplace zl:make-instance cl:make-instance))

;;; Translation of methods

;; previously defined in translate file.
;; (let ((inhibit-fdefine-warnings t))
(deftranslation zl:defmethod (form)
  (translate-flavor-method form))

(deftranslation cl:defmethod (form)
  (when (cl::flavor-method-spec-p (second form))
    (translate-flavor-method form)))

(defvar *flavor-ivars* nil)
(defsubst flavor-ivars (flavor-name)
  (gethash flavor-name *flavor-ivars* '?))

(defvar *hybrid-classes* nil)
(defvar *untranslated-flavors* nil)

(defun hybrid-class-p (name)
  (or (sys:typep-structure-or-flavor (cl:find-class name nil)
				     'cl:hybrid-class)
      (member name *hybrid-classes* :test #'eq)))

(defvar *arglist-hash-table* nil)

(defun lookup-arglist (function-name)
  (gethash function-name *arglist-hash-table*))

(defun (setf lookup-arglist) (value function-name)
  (setf (gethash function-name *arglist-hash-table*) value))

(defun generic-function-p (function)
  (typep function 'generic-function))

(defun translate-flavor-method (form)
  ;; Convert a flavors defmethod form into a clos defmethod.
  (let* ((spec (second form))
	 (flavor-name (first spec))
	 (arglist (third form))
	 (operation (car (last spec)))
	 gfun)
    ;; translate method body
    (dothese (cdddr form) form)
    (let ((class (cl:find-class flavor-name nil)))
      (when (and class
		 (not (typep class 'flavor-class)))
	(setq *untranslated-flavors*
	      (delete flavor-name *untranslated-flavors*
		      :test #'eq :count 1))))
    (unless (null
	     (setq gfun (generic-function-for-operation
			 operation
			 (or (foreign-flavor-p flavor-name)
			     (member flavor-name *untranslated-flavors*
				     :test #'eq))
			 (and (consp arglist) (null (cdr arglist))))))
      (let ((qualifiers '())
	    (body-forms (nthcdr 3 form))
	    (around-args nil))
	(unless (null (cdddr spec))
	  (push (third spec) qualifiers))
	(unless (null (cddr spec))
	  (push (second spec) qualifiers))
	(when (and (null body-forms)
		   (not (listp arglist)))
	  (if (eq arglist 'ignore)
	      (setq arglist `(&rest ignore)
		    body-forms `((declare (ignore ignore)) nil))
	      (let (other-arglist)
		(if (and (symbolp arglist)
			 (fboundp arglist)
			 (dolist (x (setq other-arglist (arglist arglist)) t)
			   (if (member x lambda-list-keywords :test #'eq)
			       (return nil))))
		    (setq body-forms `((,arglist self . ,other-arglist))
			  arglist other-arglist)
		    (setq body-forms `((apply #',arglist self list))
			  arglist `(&rest list)
			  )))))
	(cond ((equal qualifiers '(:around))
	       (setq around-args arglist)
	       (setq arglist (nthcdr 3 arglist)))
	      ((equal qualifiers '(:default))
	       (setq qualifiers nil)) )
	(when (eq operation ':init)
	  (when (or (member flavor-name *untranslated-flavors* :test #'eq)
		    (and (equal qualifiers '(:before))
			 (hybrid-class-p flavor-name)))
	    ;; Can't translate this since it must be done after
	    ;; shared-initialize, but before any other :init methods.
	    (return-from translate-flavor-method (nlam)))
	  (setq gfun 'cl:shared-initialize)
	  (when (null qualifiers)
	    (setq qualifiers '(:after)))
	  (unless (eq (car arglist) '&rest)
	    (push '&rest arglist))
	  (push 'ignore arglist))
	;; has fewer args than :print-self method
	(when (eq gfun 'cl:print-object)
	  (let ((rest-arglist (rest arglist)))
	    (cond ((null rest-arglist))
		  ((or (equal rest-arglist '(ignore ignore))
		       (equal rest-arglist '(&rest ignore)))
		   (setq arglist (list (first arglist))))
		  (t (setq arglist `(,(first arglist)
				      &aux (,(second arglist) si:*prindepth*)
				      (,(third arglist) *print-escape*)))))))
	(multiple-value-bind (body decls slot-names self-used)
	    (process-method-body flavor-name operation arglist body-forms
				 around-args)
	  (let* ((self-name (if (and (null slot-names)
				     (not self-used)
				     (not (special-variable-p flavor-name)))
				flavor-name
				'self))
		 (new-arglist
		  (if (and (consp gfun) (eq (car gfun) 'setf))
		      ;; :before or :after method might do this
		      (if (eq (first arglist) '&rest)
			  `(ignore (,self-name ,flavor-name) ,@arglist)
			  `(,(first arglist) (,self-name ,flavor-name)
			     ,@(rest arglist)))
		      `((,self-name ,flavor-name) ,@arglist)))
		 (rest-arglist (member '&rest new-arglist :test #'eq)))
	    (unless (null slot-names)
	      (setq body `((cl:with-slots ,slot-names ,self-name
			     . ,body))))
	    (when (special-variable-p self-name)
	      (push `(declare (unspecial ,self-name)) body))
	    (when (null *arglist-hash-table*)
	      (setf *arglist-hash-table* (make-hash-table :test #'eq)))
	    (if (and rest-arglist
		     (eq (second rest-arglist) 'ignore))
		(let ((gfun-arglist (if (generic-function-p gfun)
					(arglist gfun)
					(lookup-arglist gfun))))
		  ;; Try to avoid non-conforming parameter list.
		  (let ((n (position '&rest (the list new-arglist)
				     :test #'eq)))
		    (when (> (length gfun-arglist) n)
		      (setf new-arglist
			    (nconc (firstn n new-arglist)
				   (nthcdr n gfun-arglist)))
		      (push
		       `(declare
			 (ignore . ,(loop for arg in (nthcdr n gfun-arglist)
				       unless (member arg lambda-list-keywords
						      :test #'eq)
				       collect arg)))
		       body) )))
		(when (symbolp gfun)
		  (setf (lookup-arglist gfun) new-arglist)))
	    (change form `(cl:defmethod ,gfun ,@qualifiers
			    ,new-arglist
			    ,@decls
			    ,@body)))))))
  (nlam))

(defvar *operation-hash-table* nil)

(defun process-method-body (flavor-name operation arglist body-forms &optional around-args)
  (declare (values body decls slot-names self-used))
  (when (null *operation-hash-table*) (initialize-operation-table))
  (multiple-value-bind (body decls doc)
      (parse-body body-forms nil t)
    (when doc (push doc decls))
    (let ((slot-names '())
	  (self-used nil)
	  (ivars (if (null (sys:compilation-flavor flavor-name))
		     (flavor-ivars flavor-name)
		   (cdddr (sys:flavor-declaration flavor-name))))
	  )
      (declare (list slot-names))
      (flet ((instance-var-p (symbol)
			     (member symbol slot-names :test #'eq)))
	(compiler:code-walk `(function (lambda ,arglist . ,body))
			    #'(lambda (form)	; this function called on each non-local function call
				(block check-form
				  (case (car form)
				    (ticl:VARIABLE-BOUNDP
				     (compiler:cw-expression (second form))
				     (when (instance-var-p (second form))
				       (change form `(cl:slot-boundp self ',(second form)))))
				  #| ; no longer needed, SYMBOL-MACROLET now supports MULTIPLE-VALUE-SETQ.  -- DNG 4/24/89
				    (MULTIPLE-VALUE-SETQ
				     (when (= (length form) 3)
				       (let ((temp `(values . ,(second form))))
					 (compiler:cw-expression temp)
					 (when (some #'instance-var-p (cdr temp))
					   (change form `(setf ,temp ,(third form)))
					   (compiler:cw-expression (third form))
					   (return-from check-form (values form t))))))
				  |#
				    (ticl:LEXPR-FUNCALL-WITH-MAPPING-TABLE
				     (when (eq (second form) (first around-args))
				       (change form
					       (if (eq (fourth form) (third around-args))
						   `(cl:call-next-method)
						 (if (eq (fourth form) operation)
						     `(apply #'cl:call-next-method self . ,(nthcdr 4 form))
						   (if (null (nthcdr 4 form))
						       `(apply #'cl:call-next-method self (cdr ,(fourth form)))
						     `(apply #'cl:call-next-method self . ,(nthcdr 4 form))))))))
				    (ticl:FUNCALL-WITH-MAPPING-TABLE
				     (when (and (eq (second form) (first around-args))
						(or (eq (fourth form) operation)
						    (eq (fourth form) (third around-args))))
				       (change form
					       `(cl:call-next-method self . ,(nthcdr 4 form)))))
				    (ticl:CONTINUE-WHOPPER
				     (change form `(cl:call-next-method self . ,(rest form))))
				    (ticl:LEXPR-CONTINUE-WHOPPER
				     (change form `(apply #'cl:call-next-method self . ,(rest form))))
				    )
				  form))
			    ;; This function called on each free variable reference.
			    #'(lambda (x)
				(when (if (listp ivars)
					  (member x ivars :test #'eq)
					(not (special-variable-p x)))
				  (pushnew x slot-names :test #'eq))
				(when (eq x 'self)
				  (setq self-used t))
				x)))
      (values body decls (sort slot-names #'string<) self-used))
    ))

(defun special-variable-p (name)
  (and (symbolp name)
       (or (get name 'special)
	   (constantp name)
	   (eql (char (symbol-name name) 0) #\*))))

(defparameter *untranslatable-messages*
	      '(;; from vanilla-flavor
		:which-operations :operation-handled-p :send-if-handles
		;; window operations
		:ANY-TYI :ANY-TYI-NO-HANG :tyo :beep :char-aluf :clear-screen :clear-eol :draw-char
		:BITBLT :select :home-down :INSIDE-EDGES :current-font :set-current-font :GET-PANE
		:background-color :set-background-color
		:parse-font-specifier
		:set-reverse-video-p :reverse-video-p :set-cursorpos
		;; pathname operations
		:new-pathname
		))

(defun initialize-operation-table ()
  (when (null *flavor-ivars*)
    (setq *flavor-ivars* (make-hash-table :test #'eq)))
  (setq *operation-hash-table* (make-hash-table :size 200 :test #'eq))
  (dolist (x *untranslatable-messages*)
    (setf (gethash x *operation-hash-table*)
	  x))
  (dolist (x '((:print-self	cl:print-object)
	       (:describe	describe)
	       (:get		cl:get-property)
	       (:putprop	(setf cl:get-property))
	       (:remprop	cl:remove-property)
	       ;; This is for SEND; DEFMETHOD will use SHARED-INITIALIZE.
	       (:init		cl:reinitialize-instance)
	       (:tyi		read-char)
	       ;;(:tyo		write-char) ; different argument order
	       (:read-char	read-char)
	       ;;(:unread-char	unread-char) ; different argument order
	       (:fresh-line	fresh-line)
	       (:string-out	write-string)
	       (:force-output	force-output)
	       (:finish		finish-output)
	       (:clear-output	clear-output)
	       (:listen		listen)
	       (:clear-input	clear-input)
	       (:truename	truename)
	       (:fasd-form	cl:make-load-form)
	       ))
    (setf (gethash (first x) *operation-hash-table*) (second x)))
  *operation-hash-table*)

(add-initialization "Discard translator hash tables"
		    '(setq *operation-hash-table* nil *flavor-ivars* nil *arglist-hash-table* nil)
		    :full-gc)

(defun generic-function-for-operation (keyword &optional dont-add-p (maybe-setf-p t))
  ;; Given a flavor message keyword, return the corresponding generic function name.
  ;; NIL is returned if the operation should not be translated.
  (when (null *operation-hash-table*) (initialize-operation-table))
  (let ((name (symbol-name keyword))
	(x (gethash keyword *operation-hash-table*))
	temp)
    (declare (string name))
    (if x
	(if (keywordp x)
	    (let ((symbol (find-symbol name *package*)))
	      (if (and symbol (cl:generic-function-p symbol))
		  ;; user defined his own generic function for this.
		  symbol
		;; else can't translate it.
		nil))
	  x)
      (and (not dont-add-p)
	   (setf (gethash keyword *operation-hash-table*)
		 (if (and maybe-setf-p
			  (string= name "SET-" :end1 4)
			  (setq temp (find-symbol (subseq name 4) *keyword-package*))
			  (setq temp (generic-function-for-operation temp t nil)))
		     `(setf ,temp)
		   (multiple-value-bind (symbol indicator)
		       (intern name *package*)
		     (if (or (null indicator)	; symbol didn't previously exist
			     (cl:generic-function-p symbol)
			     (and (neq indicator ':inherited)
				  (not (fboundp symbol))))
			 symbol
		       (progn
			 (fresh-line *query-io*)
			 (cond ((fboundp symbol)
				(format *query-io* "~S is already defined as a non-generic function in file \"~A\"."
					symbol (sys:get-source-file-name symbol 'defun)))
			       ((eq indicator ':inherited)
				(format *query-io* "Symbol ~S is inherited from the ~A package."
					symbol (package-name (symbol-package symbol))))
			       )
			 (loop (format *query-io* "~&What is the name of the generic function for message ~s ? "
				       keyword)
			       ;; Use READ-LINE instead of READ in order to require pressing RETURN
			       ;; instead of terminating on any non-symbol character.
			       (let* ((line (read-line *query-io*))
				      (*error-output* *query-io*)
				      (answer (catch-error (read-from-string line))))
				 (when (and answer (sys:validate-function-spec answer))
				   (return answer))
				 (beep)
				 (format *query-io* "~&~S is not a valid function spec; try again."
					 (or answer line))))
			 )))))))))

;;;			FUNCTION

(deftranslation function (form)
  (if (eq (car-safe (second form)) 'lambda )
      (dothis (second form) form)
    (nlam)))

;;;		Translation of SEND

(let ((inhibit-fdefine-warnings t)) ; previously defined in Z-TO-C file.
  (deftranslation send (form)			; (SEND thing :msg arg1 arg2) ==> (msg thing arg1 arg2)
    (let ((operation (unquote-keyword (third form))))
      (cond ((not (keywordp operation)) nil)
	    ((and (eq operation ':set)
		  (= (length form) 5))
	     ;; (SEND x :SET key value)
	     (let (gfun)
	       (if (and (keywordp (setq gfun (unquote-keyword (fourth form))))
			(setq gfun (generic-function-for-operation gfun nil nil)))
		   (change form `(setf (,gfun ,(second form))
				       ,(fifth form)))
		 ;; This isn't exactly correct since the slot name will be in the wrong
		 ;; package, but it's a step in the right direction.
		 (change form `(setf (cl:slot-value ,(second form) ,(fourth form))
				     ,(fifth form))))))
	    ((and (eq operation ':send-if-handles)
		  (>= (length form) 4)
		  (keywordp (unquote-keyword (fourth form))))
	     (let ((new-form `(send ,(second form) . ,(nthcdr 3 form))))
	       (dothis new-form form)
	       (unless (eq (car-safe new-form) 'send)
		 (change form
			 `(if-handles ,new-form)))))
	    ((and (eq operation ':operation-handled-p)
		  (= (length form) 4)
		  (keywordp (unquote-keyword (fourth form))))
	     (let ((new-form `(send ,(second form) . ,(nthcdr 3 form))))
	       (dothis new-form form)
	       (unless (or (eq (car-safe new-form) 'send)
			   (not (= (length new-form) 2)))
		 (change form
			 `(argument-handled-p #',(first new-form) ,(second new-form))))))
	    (t (let ((gfun (generic-function-for-operation operation nil (= (length form) 4))))
		 (unless (null gfun)
		   (change form
			   (if (symbolp gfun)
			       (list* gfun (second form) (cdddr form))
			     (if (and (consp gfun) (eq (car gfun) 'setf))
				 ;; (SEND foo :set-bar a) ==> (SETF (bar foo) a)
				 `(setf (,(second gfun) ,(second form) . ,(cddddr form))
					,(fourth form))
			       `(funcall #',gfun ,(second form) . ,(cdddr form))
			       ))))))))))

(defun unquote-keyword (form)
  (if (and (consp form) (eq (car form) 'quote) (keywordp (second form)))
      (second form)
    form))

(defmacro if-handles (form)
  "Evaluate generic function call FORM, or return NIL if there is no applicable method.
This is used by the translator as a crude equivalent to :SEND-IF-HANDLES.
A better way is to write a default method or a method for NO-APPLICABLE-METHOD."
  (if (eq (car form) 'setf)
      `(and (argument-handled-p #'(setf ,(first (second form))) ,(second (second form)))
	    ,form)
    (let* ((function `(function ,(first form)))
	   (args (cdr form)))
      (if (compiler:trivial-form-p (first args))
	  `(and (argument-handled-p ,function ,(first args))
		,form)
	(let ((temp (gensym)))
	  `(let ((,temp ,(first args)))
	     (and (argument-handled-p ,function ,temp)
		  (,(first form) ,temp . ,(rest args)))))))))

(comment ; second version
  (defmacro if-handles (form)
    "Evaluate generic function call FORM, or return NIL if there is no applicable method.
This is used by the translator as a crude equivalent to :SEND-IF-HANDLES.
A better way is to write a default method or a method for NO-APPLICABLE-METHOD."
    (if (every #'compiler:trivial-form-p (the list (cdr form)))
	`(and (cl:handlesp #',(car form) . ,(cdr form))
	      ,form)
      (let ((temp (gensym)))
	`(with-stack-list (,temp . ,(cdr form))
	   (and (apply #'cl:handlesp #',(car form) ,temp)
		(apply #',(car form) ,temp))))))
  )

(comment					; first version
  (defmacro if-handles (form)
    "Evaluate generic function call FORM, or return NIL if there is no applicable method.
This is used by the translator as a crude equivalent to :SEND-IF-HANDLES.
A better way is to write your own method for NO-APPLICABLE-METHOD."
    ;; Actually we only want to ignore NO-APPLICABLE-METHOD errors, but that
    ;; does not yet have a distinct condition name.
    `(values (ignore-errors ,form)))
  )

(defun argument-handled-p (defn arg)
  (declare (arglist generic-function first-argument))
  "This is used by the translator as a crude equivalent to the :OPERATION-HANDLED-P message."
  (let* ((gfun (cond ((sys:typep-structure-or-flavor defn 'cl:generic-function)
		      defn)
		     ((cl:generic-function-p defn)
		      (cl:get-generic-function-object defn))
		     ((functionp defn) (return-from argument-handled-p defn))
		     (t (error "~S is not a function." defn))
		     ))
	 (arg-class (clos:class-of arg)))
    (dolist (ml (cl:generic-function-method-list gfun) nil)
      (when (cl:subclassp arg-class (car ml))
	(return t)))))

(deftranslation ticl:lexpr-send (form)
  (let ((operation (unquote-keyword (third form))))
    (when (keywordp operation)
      (let ((gfun (generic-function-for-operation operation nil nil)))
	(unless (null gfun)
	  (change form `(apply #',gfun ,(second form) . ,(cdddr form))))))))

(setf (get 'zl:lexpr-funcall 'transform)
      (get 'ticl:lexpr-send 'transform))

(deftranslation push (form)
  ;; added this so that the second argument of (PUSH x (SEND ...)) will get translated.
  (dothese (cdr form) form))


;;;		Translation of DEFFLAVOR to DEFCLASS

(defparameter *flavor-substitution-table*
	`((si:property-list-mixin . cl:property-mixin))
  "A-list of flavor names and the corresponding class name to be used instead.")

(deftranslation ticl:defflavor (form &aux outside-accessible-slots default-initargs
				          inittable-slots gettable-slots settable-slots
					  accessor-prefix doc combinations)
  (when (null *operation-hash-table*) (initialize-operation-table))
  (DESTRUCTURING-BIND (ignore name slots supers &rest options) form
    (pushnew name *untranslated-flavors* :test #'eq) ; in case the user aborts out of the translation
    (macrolet ((find-slot (slot-name option)
			  `(or (eq t ,option )
			       (member ,slot-name ,option :test #'eq )))
	       (update-list (list option)
		  `(setf ,list (if (and (consp ,option)
					(listp ,list))
				   (union (cdr ,option) ,list :test #'eq)
				 t))))
      (dolist (option options)
	(case (if (consp option) (car option) option)
	  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
	   (update-list outside-accessible-slots option))
	  ((:INITTABLE-INSTANCE-VARIABLES :INITABLE-INSTANCE-VARIABLES)
	   (update-list inittable-slots option))
	  (:GETTABLE-INSTANCE-VARIABLES
	   (update-list gettable-slots option))
	  (:SETTABLE-INSTANCE-VARIABLES
	   (update-list settable-slots option))
	  (:accessor-prefix
	   (setf accessor-prefix (cadr option)))
	  (:default-init-plist
	   (setf default-initargs (cdr option)))
	  (:INCLUDED-FLAVORS
	   ;; The distinction between component flavors and included flavors is
	   ;; probably not needed with the different class precedence ordering of CLOS.
	   (setf supers (append supers (cdr option))))
	  (:documentation
	   (dolist (x (cdr option))
	     (if (stringp x)
		 (setf doc x)
	       (let ((temp (documentation name 'defflavor)))
		 (when temp ; take advantage of what SYS::FLAVOR-HACK-DOCUMENTATION did.
		   (setq doc temp)
		   (return))))))
	  ((:init-keywords :required-instance-variables :abstract-flavor :required-methods)
	   ;;  safe to ignore these?  Might be nice to turn into a comment.
	   )
	  (:method-combination
	   (when (consp option)
	     (dolist (x (cdr option))
	       (when (consp x)
		 (let ((combiner (intern (string (car x)) "TICLOS")))
		   (dolist (operation (cddr x))
		     (let* ((gfun (generic-function-for-operation operation nil nil))
			    (meth (sys:fdefinition-safe `(:method ,name ,(car x) ,operation) t))
			    (arglist (cond (meth (cons (intern "OBJECT" *package*)
						       (cdr (arglist meth))))
					   ((cl:generic-function-p gfun)
					    (arglist gfun t))
					   (t `(,(intern "OBJECT" *package*) &rest ,(intern "ARGS" *package*))))))
		     (pushnew `(cl:defgeneric ,gfun
						  ,arglist
				 (:method-combination ,combiner ,(case (second x)
								     (:base-flavor-first :most-specific-last)
								     (:base-flavor-last :most-specific-first)
								     (t (second x)))))
			      combinations :test #'equal))))))))
	  (t (cerror "Continue, ignoring it."
		     "Don't know what to do with DEFFLAVOR option ~S."
		     option))
	  ))
      (when (null (sys:compilation-flavor name))
	(let ((slot-names (mapcar #'(lambda (x) (if (atom x) x (car x)))
				  slots)))
	  (dolist (super supers)
	    (let ((x (flavor-ivars super)))
	      (when (consp x)
		(setq slot-names (union x slot-names :test #'eq)))))
	  (setf (flavor-ivars name) slot-names)))
      ;;  settable implies inittable
      (cond ((eq inittable-slots 't))
	    ((eq settable-slots 't)
	     (setf inittable-slots 't))
	    (t (setf inittable-slots (union inittable-slots settable-slots))))
      (let ((slots (cl:collect-body
		     (dolist (slot slots)
		       (let ((slot-name (if (consp slot) (car slot) slot)))
			 (cl:collect
			   `(,slot-name
			     ,@(if (and (consp slot) (cdr slot)) `(:initform ,(cadr slot)))
			     ,@(if (find-slot slot-name outside-accessible-slots )
				   `(:accessor ,(intern (concatenate 'string
								     (if accessor-prefix
									 (symbol-name accessor-prefix)
								       (concatenate 'string
										    (symbol-name name)
										    "-"))
								     (symbol-name slot-name))
							*package*)))
			     ,@(if (find-slot slot-name inittable-slots  )
				   `(:initarg ,(intern (symbol-name slot-name) *keyword-package*)))
			     ,@(if (find-slot slot-name gettable-slots)
				   (let ((op (generic-function-for-operation
					       (intern (symbol-name slot-name) *keyword-package*)
					       nil nil)))
				     (if (find-slot slot-name settable-slots)
					 (let ((setter (intern (concatenate 'string "SET-" (symbol-name slot-name))
							  *keyword-package*)))
					   (unless (gethash setter *operation-hash-table*)
					     (setf (gethash setter *operation-hash-table*)
						   `(setf ,op)))
					   `(:accessor ,op))
				       `(:reader ,op)))
				 (if (find-slot slot-name settable-slots)
				     `(:writer ,(generic-function-for-operation
						  (intern (concatenate 'string "SET-" (symbol-name slot-name))
							  *keyword-package*)
						  nil t))))
			     )))))))
	(let* ((substitution-alist '())
	       (metaclass (dolist (super supers nil)
			    (when (symbolp super)
			      (when (and (foreign-flavor-p super)
					 (not (hybrid-class-p super)))
				;; Including a flavor that appears to be defined outside of the
				;; program being translated.
				(let ((temp (assoc super *flavor-substitution-table* :test #'eq)))
				  (if temp
				      ;; There is a class we can substitute for it.
				      (pushnew temp substitution-alist :test #'equal)
				    ;; Else this will need to be a hybrid class so it can inherit the flavor.
				    (progn (pushnew name *hybrid-classes* :test #'eq)
					   (setq substitution-alist nil)
					   (return '((:metaclass cl:hybrid-class))))))))))
	       (new-form `(cl:defclass ,name ,(sublis substitution-alist supers :test #'eq)
			    ,slots
			    ,@(and default-initargs `((:default-initargs ,@default-initargs)))
			    ,@(and doc `((:documentation ,doc)))
			    ,@metaclass
			    )))
	  (unless (null combinations)
	    (setq new-form `(progn ,new-form . ,combinations)))
	  (change form new-form))
	(setq *untranslated-flavors* (delete name (the list *untranslated-flavors*) :test #'eq :count 1))
	(nlam)))))

(defun foreign-flavor-p (name)
  ;; Is this the name of a flavor which appears to be defined outside the program being translated?
  (and (symbolp name)
       (let ((fl (sys:get-flavor name)))
	 (and (not (null fl))
	      (not (eq (si:flavor-definition-package fl)
		       *package*)
		   )))))

;;;			DEFWRAPPER

(deftranslation DEFWRAPPER (form)
  (destructuring-bind ((flavor-name operation) (arglist . body) &rest wrapper)
		      (cdr form)
    (change form
	    `(cl:defmethod ,(generic-function-for-operation operation nil (consp arglist))
			       :around
	                       ((self ,flavor-name) . ,arglist)
	       (declare (unspecial self))
	       ,(progv (list body) '(((cl:call-next-method)))
		  (apply #'progn wrapper))))))


;;;			DEFWHOPPER

(deftranslation DEFWHOPPER (form)
  (let ((exp (macroexpand-1 form)))
    (dothis exp form)
    (change form exp)))

;;;			DEFUN-METHOD

(deftranslation DEFUN-METHOD (form)
  (destructuring-bind (function-spec flavor-name arglist &body body-forms)
		      (cdr form)
    (multiple-value-bind (body decls slot-names)
	(process-method-body flavor-name nil arglist body-forms)
      (unless (null slot-names)
	(setq body `((cl:with-slots ,slot-names (the ,flavor-name self)
		       . ,body))))
      (change form `(defun ,function-spec ,arglist
		      ,@decls
		      ,@body)))))


(deftranslation COMPILE-FLAVOR-METHODS (form) ; no CLOS equivalent
  (change form `(comment ,(copy-list form)))
  t)
