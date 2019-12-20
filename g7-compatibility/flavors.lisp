;; -*- Mode:Common-Lisp; Package:(G7C LISP); Fonts:(MEDFNT HL12B HL12I MEDFNT MEDFNTB); Patch-file:T; Base:10 -*-

;;; "Genera" and "Symbolics" are trademarks of Symbolics, Inc.
;;; "Explorer" is a trademark of Texas Instruments.


;;; REVISIONS:
;;; 20 Mar 89 Mac - Revised SCL:DEFFLAVOR so that :READABLE, :WRITEABLE, :SETTABLE, and :GETTABLE
;;;                    now have a -INSTANCE-VARIABLES suffix.  

;;; GENERIC FUNCTIONS
(DEFUN DEFINE-GENERIC-FUNCTION (function-name)
   "Creates a function definition that translates a call to (FUNCTION-NAME object args...) into
a call to (send object :FUNCTION-NAME args...)"
   (pushnew function-name sys:*all-generic-function-names*)
   (ticl:fdefine function-name
		 #'(lambda (object &optional &rest args)
		     (apply object (intern function-name sys:*keyword-package*) args))))

(DEFINE-GENERIC-FUNCTION 'DBG:BUG-REPORT-RECIPIENT-SYSTEM)
(DEFINE-GENERIC-FUNCTION 'DBG:BUG-REPORT-DESCRIPTION)
(DEFINE-GENERIC-FUNCTION 'DBG:DOCUMENT-PROCEED-TYPE)
(DEFINE-GENERIC-FUNCTION 'DBG:INITIALIZE-SPECIAL-COMMANDS)
(DEFINE-GENERIC-FUNCTION 'SYS:PROCEED)
(DEFINE-GENERIC-FUNCTION 'DBG:PROCEED-TYPE-P)
(DEFINE-GENERIC-FUNCTION 'DBG:PROCEED-TYPES)
(DEFINE-GENERIC-FUNCTION 'DBG:REPORT)
(DEFINE-GENERIC-FUNCTION 'DBG:REPORT-STRING)
(DEFINE-GENERIC-FUNCTION 'DBG:SPECIAL-COMMAND)


(proclaim '(inline scl:send))
(DEFUN SCL:SEND (object operation &rest args)
   "Send a message to OBJECT, with operation OPERATION and ARGUMENTS.  If the object is SELF,
a special fast calling sequence is generated."
   (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0)))
   (apply object operation args))

(proclaim '(inline scl:lexpr-send))
(DEFUN SCL:LEXPR-SEND (&rest args)
   "Send a message to OBJECT, with operation OPERATION and ARGUMENTS.  If the object is SELF,
a special fast calling sequence is generated.  The las argument to the call is a list."
   (declare (arglist object operation &rest args)
	    (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0)))
   (apply args))

(DEFUN SYS:%INSTANCE-FLAVOR (flavor-instance)
   "Returns the flavor instance describing the flavor of which FLAVOR-INSTANCE is an instance.
For example,  (sys:%instance-flavor (make-instance 'foo)) => #<SYS:FLAVOR FOO ...>"
     (get (type-of flavor-instance) 'sys:flavor))


;;; Note this by itself does not allow typep of flavor:vanilla to equal typep of sys:vanilla-flavor
;;; Don't know why yet (package definition make a difference?) so added deftype for now to resolve
(TICL:DEFFLAVOR FLAVOR:VANILLA
	    ()
	    (sys:vanilla-flavor)
  :ALIAS-FLAVOR)

(DEFTYPE FLAVOR:VANILLA () 'sys:vanilla-flavor)

SYS:
(DEFMACRO SCL:DEFFLAVOR (name instance-variables component-flavors &rest options)
 "INSTANCE-VARIABLES can be symbols, or lists of symbol and initialization.
 COMPONENT-FLAVORS are searched from left to right for methods,
  and contribute their instance variables.
 OPTIONS are:
  (:GETTABLE-INSTANCE-VARIABLES v1 v2...) 
  (:SETTABLE-INSTANCE-VARIABLES v1 v2...)
  (:REQUIRED-INSTANCE-VARIABLES v1 v2...)
  (:REQUIRED-METHODS m1 m2...)
  (:REQUIRED-FLAVORS f1 f2...)
  (:INITTABLE-INSTANCE-VARIABLES v1 v2...)
  (:INIT-KEYWORDS k1 k2...)
  (:DEFAULT-INIT-PLIST k1 v1 k2 v2...)
  (:DEFAULT-HANDLER function)
  (:INCLUDED-FLAVORS f1 f2...)
  :NO-VANILLA-FLAVOR
  (:ORDERED-INSTANCE-VARIABLES v1 v2...)
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES v1 v2...)
  (:ACCESSOR-PREFIX sym)
  (:METHOD-ORDER m1 m2...)
  (:METHOD-COMBINATION (type order operation1 operation2...)...)
  (:DOCUMENTATION <args>...)
  (:SPECIAL-INSTANCE-VARIABLES <variables>)
  :ABSTRACT-FLAVOR 
  :ALIAS-FLAVOR"
  ;;There may be more.
  (let ((copied-options     (copy-list options))
	(outside-accessible nil))
    ;; convert Symbolics options into Explorer options
    (do ((option copied-options (rest option)))
	((null option))
      (if (consp (first option))
	  ;; then we have a keyword with args
	  (case (first (first option))
	    (:writable-instance-variables
	                (setf (first (first option)) :settable-instance-variables)
			(setf outside-accessible (cons :outside-accessible-instance-variables
						       (copy-list (rest (first option))))))
	    (:readable-instance-variables
	                (setf (first (first option)) :gettable-instance-variables))
	    (:conc-name (setf (first (first option)) :accessor-prefix))
	    (:method-combination
	                (do* ((combo-specs    (rest (first option)) (cddr combo-specs))
			      (method-name    (first  combo-specs)   (first combo-specs))
			      (combo-name     (second combo-specs)   (second combo-specs))
			      (new-combo-specs () ))                        ; accumulated converted specs
			     ((null combo-specs)                            ; stop when no more specs to convert
			      (setf (rest (first option)) new-combo-specs)) ; replace :method-combination args
			  (if (consp combo-name)
			      ;; then we have (:CASE :MOST-SPECIFIC-foo)
			      (progn
				(setf combo-name 
				      (substitute :base-flavor-first :most-specific-last  combo-name))
				(setf combo-name 
				      (substitute :base-flavor-last  :most-specific-first combo-name))
				(push (append combo-name (list method-name)) new-combo-specs))
			      ;; else we have :combo-name
			      (push  (list combo-name :base-flavor-last method-name) new-combo-specs))))
	    ;; Commenting this out requires the use of the defflavor1 code which follows, otherwise we do not
	    ;; handle the :method-order option
	    ;;(:method-order (delete (first option) copied-options))
	    );;case
	  ;;else we have a stand alone keyword
	  (case (first option)
	    (:writable-instance-variables
	               (setf (first option) :settable-instance-variables)
		       (setf outside-accessible :outside-accessible-instance-variables))
	    (:readable-instance-variables
	               (setf (first option) :gettable-instance-variables))))
      );;do
    (when (not (null outside-accessible))
      ;; then there are some writable instance variables, so make them generic...sort of
      (push outside-accessible copied-options))

    ;; the remainder of this code is the original SYS:DEFLAVOR body
    `(progn
       (eval-when (load eval)
	  (scl:defflavor2 ',name ',instance-variables ',component-flavors ',copied-options))
       (eval-when (compile)
	  (if (just-compiling)
	    (let ((*just-compiling* t))
	      (scl:defflavor2 ',name ',instance-variables ',component-flavors ',copied-options)
	      (compose-automatic-methods (compilation-flavor ',name)))
	    (compose-automatic-methods (get ',name 'flavor))))
       (eval-when (eval) (compose-automatic-methods (get ',name 'flavor)))
       (eval-when (load eval)
	  ,@(do ((vs
		  (do ((opts options (cdr opts)))
		      ((null opts)
		       nil)
		    (and (consp (car opts))
		       (eq (caar opts) :outside-accessible-instance-variables)
		       (return (cdar opts)))
		    (and (eq (car opts) :outside-accessible-instance-variables)
		       (return
			(mapcar #'(lambda (x)
				    (if (atom x)
				      x
				      (car x)))
				instance-variables))))
		  (cdr vs))
		 (prefix
		  (or (cadr (assq-careful :accessor-prefix options)) (string-append name "-")))
		 (ords
		  (do ((opts options (cdr opts)))
		      ((null opts)
		       nil)
		    (and (consp (car opts)) (eq (caar opts) :ordered-instance-variables)
		       (return (cdar opts)))
		    (and (eq (car opts) :ordered-instance-variables)
		       (return
			(mapcar #'(lambda (x)
				    (if (atom x)
				      x
				      (car x)))
				instance-variables)))))
		 (res nil
		  (cons
		   `(defsubst ,(intern1 (string-append prefix (car vs))) (,name)
		      (declare (function-parent ,name))
		      ,(if (member (car vs) ords :test #'eq)
			 `(%instance-ref ,name
					 ,(1+ (position (car vs) (the list ords) :test #'eq)))
			 `(symeval-in-instance ,name ',(car vs))))
		   res)))
		((null vs)
		 res)))
       ,@(make-run-time-alternative-defflavors name
					       (or
						(cdr
						 (assq-careful :run-time-alternatives options))
						(cdr (assq-careful :mixture options))))
       ',name)))

;;; JPR.

sys:
(defun scl:defflavor2 (name instance-variables component-flavors copied-options)
  (cond
    ((and (variable-boundp file-warnings-datum) file-warnings-datum)
     (object-operation-with-warnings (name)
	(compiler:warn-on-errors ('flavor-definition-error "Error in flavor definition")
	   (scl:defflavor1 name instance-variables component-flavors copied-options))))
    (t (scl:defflavor1 name instance-variables component-flavors copied-options))))

;;; This code provides a modification to this system function so that the Explorer version of defflavor
;;; handles the :method-order option.  NOTE: This code has NOT been tested, Patrick D. provided it, but it has not been run yet.
;;; This should become a system patch soon.
SYS:
(DEFUN scl:DEFFLAVOR1 (flavor-name instance-variables component-flavors options &aux ffl already-exists instv
	      identical-components gettable settable inittable special-ivs old-special-ivs
	      old-default-handler old-default-init-plist old-local-ivs old-inittable-ivs
	      old-init-kwds old-instance-area-function old-required-init-keywords init-keywords
	      includes meth-comb new-plist (pl (locf new-plist))
	      (default-cons-area (if *just-compiling*
				   default-cons-area
				   *flavor-area*)))
  (or *just-compiling* (record-source-file-name flavor-name 'defflavor))
  (without-interrupts
   (cond
     ((and (not *just-compiling*) (not (member flavor-name *all-flavor-names* :test #'eq)))
      (push flavor-name *all-flavor-names*)
      ;; Push on the name without the package prefix.
      (vector-push-extend (cons (symbol-name flavor-name) flavor-name) *all-flavor-names-aarray*)
      ;; Push on the name with the package prefix.
      (vector-push-extend
       (cons (string-append (package-name *package*) ":" (symbol-name flavor-name)) flavor-name)
       *all-flavor-names-aarray*)
      ;; Array is no longer sorted.
      (store-array-leader () *all-flavor-names-aarray* 1))))
  ;; Analyze and error check the instance-variable and component-flavor lists
  (setq instv (mapcar #'(lambda (x)
			  (if (atom x)
			    x
			    (car x)))
		      instance-variables))
  (dolist (iv instv)
    (if (or (null iv) (not (symbolp iv)))
      (ferror () "~:S, which is not a symbol, was specified as an instance variable" iv)))
  (dolist (cf component-flavors)
    (if (or (null cf) (not (symbolp cf)))
      (ferror () "~:S, which is not a symbol, was specified as a component flavor" cf)))
  ;;Check for obsolete component flavors here
  (check-obsolete-flavors component-flavors "component")
  ;; Certain properties are inherited from the old property list, while
  ;; others are generated afresh each time from the defflavor-options.
  (cond
    ((and (setq already-exists (compilation-flavor flavor-name)) *use-old-flavor-info*)
     (dolist (prop defflavor1-preserved-properties)
       (setf (get pl prop) (getf (flavor-plist already-exists) prop)))))
  ;; First, parse all the defflavor options into local variables so we can see
  ;; whether the flavor is being redefined incompatibly.
  (do ((l options (cdr l))
       (option)
       (args))
      ((null l))
    (if (atom (car l))
      (setq option (car l)
	    args ())
      (setq option (caar l)
	    args (cdar l)))
    (case option
      (:gettable-instance-variables
       (validate-instance-variables-spec args instv flavor-name option)
       (setq gettable (union gettable (or args instv) :test #'eq)))
      (:settable-instance-variables
       (validate-instance-variables-spec args instv flavor-name option)
       (setq settable (union settable (or args instv) :test #'eq)))
      ((:inittable-instance-variables :initable-instance-variables)
       (validate-instance-variables-spec args instv flavor-name option)
       (setq inittable (union inittable (or args instv) :test #'eq)))
      (:special-instance-variables
       (validate-instance-variables-spec args instv flavor-name option)
       (setq special-ivs (union special-ivs (or args instv) :test #'eq)))
      (:init-keywords (setq init-keywords (union init-keywords args :test #'eq)))
      (:included-flavors (setq includes (union includes args :test #'eq))
			 (check-obsolete-flavors args "included"))
      (:no-vanilla-flavor (setf (get pl option) t))
      (:ordered-instance-variables
       ;;Don't validate.  User may reasonably want to specify non-local instance
       ;;variables, and any bogus names here will get detected by COMPOSE-FLAVOR-COMBINATION
       ;;(VALIDATE-INSTANCE-VARIABLES-SPEC ARGS INSTV FLAVOR-NAME OPTION)
       (setf (get pl :ordered-instance-variables) (or args instv)))
      (:outside-accessible-instance-variables
       (validate-instance-variables-spec args instv flavor-name option)
       (setf (get pl :outside-accessible-instance-variables)
	     (union (get pl :outside-accessible-instance-variables) (or args instv) :test #'eq)))
      (:method-combination (setq meth-comb (nunion meth-comb args :test #'equal) ))
      (:default-handler (setf (get pl option) (car args)))
      ((:required-instance-variables :required-methods :required-flavors :required-init-keywords)
       (setf (get pl option) (union args (get pl option) :test #'eq))
       (when (eq option :required-flavors)
         (check-obsolete-flavors (get pl ':required-flavors) "required")))
      ((:documentation :default-init-plist :select-method-order :accessor-prefix)
       (setf (get pl option) args))
      (:method-order (setf (get pl :select-method-order) args))
      (:alias-flavor (setf (get pl :alias-flavor) t))
      (:abstract-flavor (setf (get pl :abstract-flavor) t))
      (:instance-area-function (setf (get pl :instance-area-function) (car args)))
      (:instantiation-flavor-function (setf (get pl :instantiation-flavor-function) (car args)))
      ((:run-time-alternatives :mixture) (setf (get pl :run-time-alternatives) args)
       (setf (get pl :instantiation-flavor-function) 'choose-run-time-alternative)
       (setf (get pl 'run-time-alternative-alist)
	     (make-run-time-alternative-alist flavor-name args)))
      (otherwise (ferror () "~S is not a known DEFFLAVOR option." option))))
  ;; All settable instance variables should also be gettable and inittable.
  (dolist (v settable)
    (or (member v gettable :test #'eq) (push v gettable))
    (or (member v inittable :test #'eq) (push v inittable)))
  ;; See whether there are any changes in component flavor structure from last time
  (setq identical-components
	(and already-exists *use-old-flavor-info*
	   (equal component-flavors (flavor-depends-on already-exists))
	   (equal includes (flavor-includes already-exists))
	   (equal (get pl :required-flavors)
		  (getf (flavor-plist already-exists) :required-flavors))))
  (and already-exists
     (setq old-special-ivs (flavor-special-instance-variables already-exists)
	   old-default-handler (getf (flavor-plist already-exists) :default-handler)
	   old-default-init-plist (getf (flavor-plist already-exists) :default-init-plist)
	   old-local-ivs (flavor-local-instance-variables already-exists)
	   old-inittable-ivs (flavor-inittable-instance-variables already-exists)
	   old-instance-area-function (flavor-get already-exists :instance-area-function)
	   old-required-init-keywords (flavor-get already-exists :required-init-keywords)
	   old-init-kwds (flavor-init-keywords already-exists)))
  ;; If the flavor is being redefined, and the number or order of instance$variables
  ;; is being changed, and this flavor or any that depends on it
  ;; has a select-method table (i.e. has probably been instantiated), give a warning
  ;; and disconnect from the old FLAVOR defstruct so that old instances will
  ;; retain the old information.  The instance variables can get changed either
  ;; locally or by rearrangement of the component flavors.
  (and already-exists
     (if (and *use-old-flavor-info*
	 (equal (get pl :ordered-instance-variables)
		(getf (flavor-plist already-exists) :ordered-instance-variables))
	 (or (equal (flavor-local-instance-variables already-exists) instance-variables)
	    (equal
	     (mapcar #'(lambda (x)
			 (if (atom x)
			   x
			   (car x)))
		     (flavor-local-instance-variables already-exists))
	     instv))
	 (eq (get pl :alias-flavor) (flavor-get already-exists :alias-flavor))
	 (or identical-components
	    (equal (flavor-relevant-components already-exists component-flavors includes)
		   (flavor-relevant-components already-exists (flavor-depends-on already-exists)
					       (flavor-includes already-exists)))))
       (if *just-compiling*
	 (setq already-exists (flavor-redefinition-for-compilation already-exists ())))
       (if *just-compiling*
	 (setq already-exists (flavor-redefinition-for-compilation already-exists t))
	 (setq already-exists (perform-flavor-redefinition flavor-name)))))
  (when (get pl :alias-flavor)
    (if (cdr component-flavors)
      (flavor-warn flavor-name 'alias-flavor-multiple-components :impossible
		   "This alias flavor has more than one component."))
    (unless component-flavors
      (flavor-warn flavor-name 'alias-flavor-multiple-components :impossible
		   "This alias flavor has no component to be the alias of."))
    (if instance-variables
      (flavor-warn flavor-name 'alias-flavor-multiple-components :impossible
		   "This alias flavor has instance variables; they will be ignored.")))
  ;; Make the information structure unless the flavor already exists.
  (let ((fl
	 (or already-exists (and (not *just-compiling*) (get flavor-name 'undefined-flavor))
	    (make-flavor flavor-name flavor-name))))
    (setf (flavor-package fl) *package*)
    (setf (flavor-local-instance-variables fl) instance-variables)
    (setf (flavor-depends-on fl) component-flavors)
    (let ((ovec (flavor-component-mapping-table-vector fl)))
      (setf (flavor-plist fl) new-plist)
      (if ovec
	(setf (flavor-component-mapping-table-vector fl) ovec)))
    (if gettable
      (setf (flavor-gettable-instance-variables fl) gettable))
    (if settable
      (setf (flavor-settable-instance-variables fl) settable))
    (if special-ivs
      (setf (flavor-special-instance-variables fl) special-ivs))
    (setf (flavor-inittable-instance-variables fl)
	  (loop for v in inittable collect (cons (corresponding-keyword v) v)))
    (setf (flavor-init-keywords fl) init-keywords)
    (setf (flavor-includes fl) includes)
    ;; This can't be computed for real until flavor composition,
    ;; but this at least contains some of the right ones.
    (setf (flavor-unmapped-instance-variables fl) (flavor-known-unmapped-instance-variables fl))
    ;; First remove old method-combination declarations, then add new ones
    (dolist (mte (flavor-method-table fl))
      (cond
	((loop for decl in meth-comb never (member (car mte) (cddr decl) :test #'eq))
	 (setf (second mte) ()) (setf (third mte) ()))))
    (dolist (decl meth-comb)
      (let ((type (car decl)) (order (cadr decl)) elem)
	    ;; Don't error-check TYPE now, its definition might not be loaded yet
	(dolist (msg (cddr decl))
	  (or (setq elem (assoc msg (flavor-method-table fl) :test #'eq))
	     (push (setq elem (list* msg () () ())) (flavor-method-table fl)))
	  (setf (second elem) type)
	  (setf (third elem) order))))
    (if *just-compiling*
      (compilation-define-flavor flavor-name fl)
      ;; Make this a depended-on-by of its depends-on, or remember to do it later in
      ;; the case of depends-on's not yet defined.
      (progn
	(dolist (component-flavor component-flavors)
	  (without-interrupts
	   (cond
	     ((setq ffl (get component-flavor 'flavor))
	      (or (member flavor-name (flavor-depended-on-by ffl) :test #'eq)
		 (push flavor-name (flavor-depended-on-by ffl))))
	     (t (push (cons component-flavor flavor-name) *flavor-pending-depends*)))))
	(dolist (included-flavor (flavor-includes fl))
	  (without-interrupts
	   (cond
	     ((setq ffl (get included-flavor 'flavor))
	      (or (member flavor-name (flavor-depended-on-by ffl) :test #'eq)
		 (push flavor-name (flavor-depended-on-by ffl))))
	     (t (push (cons included-flavor flavor-name) *flavor-pending-depends*)))))
	(without-interrupts
	 (dolist (x *flavor-pending-depends*)
	   (cond
	     ((eq (car x) flavor-name)
	      (or (member (cdr x) (flavor-depended-on-by fl) :test #'eq)
		 (push (cdr x) (flavor-depended-on-by fl)))
	      (setq *flavor-pending-depends*
		    (delete x (the list *flavor-pending-depends*) :test #'eq))))))
	(setf (get flavor-name 'flavor) fl)
	(remprop flavor-name 'undefined-flavor)
	(if (and already-exists (not identical-components))
	  (perform-flavor-method-only-redefinition flavor-name)
	  ;; If the methods and instances are ok but other things have changed, notice that too.
	  (or
	   (and (equal old-special-ivs (flavor-special-instance-variables fl))
	      (equal old-default-init-plist (getf (flavor-plist fl) :default-init-plist))
	      (equal old-local-ivs (flavor-local-instance-variables fl))
	      ;; Get a warning every time, if there is a variable
	      ;; that is globally special but not in a :SPECIAL-INSTANCE-VARIABLES
	      (not
	       (dolist (iv (flavor-local-instance-variables fl))
		;; Elements can be lists (var init)
		 (if (consp iv)
		   (setq iv (car iv)))
		 (and (get iv 'special)
		    (not (member iv (flavor-special-instance-variables fl) :test #'eq))
		    (return t))))
	      (equal old-inittable-ivs (flavor-inittable-instance-variables fl))
	      (equal old-default-handler (getf (flavor-plist fl) :default-handler))
	      (equal old-instance-area-function (flavor-get fl :instance-area-function))
	      (equal old-required-init-keywords (flavor-get fl :required-init-keywords))
	      (equal old-init-kwds (flavor-init-keywords fl)))
	   (perform-flavor-bindings-redefinition flavor-name)))
	(flavor-hack-documentation flavor-name))
      ;; Now, if the flavor was redefined in a way that changes the methods but doesn't
      ;; invalidate old instances, we have to propagate some changes.
      ;; If someone depends on this flavor, which wasn't defined until now, link them up.
      ;; If that flavor was flavor-composed, recompose it now.
      ;; Likewise for its includes
      )
    flavor-name))

(DEFVAR *DAEMON-TYPES* '(:before :after :around :inverse-around :case :default :or :and
			    :override :progn :list :inverse-list :pass-on :append :nconc))

SYS:
(DEFMACRO SCL:DEFMETHOD (spec lambda-list . body)
  "(DEFMETHOD (flavor-name [daemon-type] operation [:case-sub-operation]) lambda-list . body)
Defines the method for flavor: flavor-name for the message operation,
Daemon-type can be one of: :BEFORE :AFTER :AROUND :INVERSE-AROUND :CASE :DEFAULT :OR :AND :OVERRIDE
:PROGN :LIST :INVERSE-LIST :PASS-ON :APPEND :NCONC.
:case-sub-operation must be provided for :CASE deamon-type,it is illegal otherwise.
Symbolics syntax for SCL:DEFMETHOD is also accepted."
   (progn
     (when (or (keywordp (first spec))	           ; Genera 7 spec => (:method flavor-name ...)
	       (not (keywordp (second spec))))	   ; Genera 7 spec => (generic-function flavor-name ...)
       ;; then we have a Genera 7 DEFMETHOD spec to convert
       (when (and (not (keywordp (first spec)))
		  (null (third spec)))	           ; Only handle the simple cases
	 ;; then we must also define a generic function for this method
	 (g7c:define-generic-function (first spec)))
       (setf spec (if (null (cddr spec))
		        ;; then no options were specified, so this is a primary method
		      (list (second spec)                                  ; put flavor name first
			    (intern (first spec) sys:*keyword-package*))   ; put method name second

		        ;; else there is at least one option, so it must precede the method name
		      (list* (second spec)                                 ; put flavor name first
			     (if (member (third spec) g7c:*daemon-types*)  ; if not std daemon type assume :case
				 (third spec)	                           ; put daemon name second
				 :case)
			     (intern (first spec) sys:*keyword-package*)   ; put method name third
			     (if (not (member (third spec) g7c:*daemon-types*))
			         (cons (third spec) (cdddr spec))          ; assume case type
			         (cdddr spec))))))                         ; put any other info last
     
     ;; normal Explorer DEFMETHOD code follows
     (let ((class-name (car spec))
	   (function-spec (cons :method spec))
	   fl)
       `(progn
       ,(and (just-compiling) (compilation-flavor class-name t)
	     (neq class-name 'vanilla-flavor);This kludge avoids bootstrapping problems!
	   `(eval-when (compile)
	       (let ((*just-compiling* t))
		 (flavor-notice-method ',function-spec))))
       ,(cond
	  ((and (symbolp lambda-list) (not (null lambda-list)) (null body))
	   `(sys:fdefine-for-defmethod ',function-spec ',lambda-list t))
	  ((setq fl (compilation-flavor class-name t))
	   (if (flavor-get fl :alias-flavor)
	     (ferror () "Attempt to define ~S; the flavor is an alias flavor."
		     (cons :method spec)))
	   `(defun ,function-spec ,(method-argument-list lambda-list function-spec)
	      (declare (:self-flavor ,class-name))
	      ,@body))
	  (t
	     (ferror () "~S is not a flavor" (car spec))))))))
  

(DEFUN SYS:DEFINE-CHARACTER-STYLE-FAMILIES (device character-set &rest plists)
  "dummy version of the Symbolics function"
   (declare (ignore device character-set plists))
   (ignore))


(DEFMACRO SCL:DEFUN-IN-FLAVOR ((function-name flavor-name) arglist &body body)
   `(ticl:defun-method ,function-name ,flavor-name ,arglist ,@body))


