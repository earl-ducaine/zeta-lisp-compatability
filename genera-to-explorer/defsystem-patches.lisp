;;; -*- Mode:Common-Lisp;Package:SYSTEM;Base:10;Patch-file:T -*-


;;; From: TILDE::"DMEYER@HOME" 21-DEC-1987 13:01

	
;;; This patch will make the :do-components
;;; transformation do make-system's on specified systems
;;; instead of just hauling in all those system's
;;; transformations.  It also completes all
;;; transformations preceding :do-components before
;;; starting the component systems.  Since this is doing
;;; make-system's on the component systems, patches are
;;; loaded for them if needed.
 
;;; Anyone who was counting on component system's
;;; transformations being around local to the parent
;;; system at make-system time, won't find them there any
;;; longer.  -dkm 11/87
 
;;; from DEFS file
 
;;; define a new make-system special to hold make-system keywords
(define-make-system-special-variable *make-system-keywords* nil)
 
 
;;; from DEFSYS file
 
;;; change the definition of do-components-internal to no
;;; longer be a dummy -- point to a real function

(define-simple-transformation do-components-internal do-components-1 true nil nil
			      ("Make Component Systems" "Making Component Systems" "Made Component Systems")
			      T T)
 
;;; New function which does make-system's on each of the component systems.
(defun do-components-1 (&rest ignore)
  "Do a make-system on each component system for this system"
  (loop for system in (system-component-systems *system-being-made*)
	do (apply 'make-system system *make-system-keywords*)))
 
;;; from MAKSYS file
 
;;; Save the specified keywords to pass along to interior
;;; make-systems.  See 1 line change below.
 
(DEFUN MAKE-SYSTEM (SYSTEM &REST KEYWORDS &AUX *SOMETHING-LOADED*)
  "Operate on the files of the system SYSTEM.
Most commonly used to compile or load those files which need it.
Keywords are not followed by values.
Commonly used keywords include:
 
 :COMPILE            - recompile source files that have been changed since last make-system.
 '(:COMPILE :compiler-keyword1 value1 :compiler-keyword2 value2 ...)
                     - pass these options to the compiler.
 :RECOMPILE          - recompile and reload all files for this system.
 '(:RECOMPILE :compiler-keyword1 value1 :compiler-keyword2 value2 ...)
                     - pass these options to the compiler.
 :NOLOAD             - don't load compiled files.
 :RELOAD             - load even files already loaded.
 :SELECTIVE          - ask user about each file individually.
 :NOCONFIRM          - do not ask for confirmation of make-system files.
 :NOWARN             - do not prompt for ANY confirmations, including loader redefinition warnings.
 :SILENT             - don't print lists of files or loader warnings on the terminal at all.
 :NO-INCREMENT-PATCH - don't increment the patch version number of a patchable system.
 :INCREMENT-PATCH    - do increment the patch version number.
 :NO-LOAD-PATCHES    - do not load patches for patchable system being loaded.
 :NO-RELOAD-SYSTEM-DECLARATION - don't reload the file that contains the DEFSYSTEM.
 :PRINT-ONLY         - don't load or compile anything, just say what needs to be done.
 :DESCRIBE           - say when files were compiled or loaded, etc.
 :BATCH              - write a file containing any warnings produced by compilation.
                       Just load the file, as lisp code, to reload the warnings.
 :DEFAULTED-BATCH    - like :BATCH except warnings file is defaulted instead of asked for.
 :DO-NOT-DO-COMPONENTS - do not include systems defined by :component-systems.
 :RECORD             - record the file version numbers for the current system
 (:VERSION [num])    - remake an old major version of a system if that previous 
                       system was recorded via the :RECORD option.
 :SAFE               - in determining source later than object, go by the creation date.
                       The default depends on :OUTPUT-VERSION from the DEFSYSTEM
 :NOOP               - this option is ignored."
;**********************
 
  (PROGW *MAKE-SYSTEM-SPECIAL-VARIABLES*
    (UNWIND-PROTECT
      (CATCH 'EXIT-MAKE-SYSTEM
	(SETQ KEYWORDS (COPY-LIST KEYWORDS)	   ;get copy of &rest arg to be safe
	      *make-system-keywords* keywords)	   ;save keywords for sub make-systems -dkm 11/87
	(FIND-SYSTEM-NAMED SYSTEM NIL NIL KEYWORDS)	   ;be sure the defsystem is loaded
	(MAYBE-RELOAD-SYSTEM-DECLARATION SYSTEM KEYWORDS)  ; and that it is current
 
	;;initialize some make-system-special-variables
	;get the real system, in case new one loaded
	(SETQ *SYSTEM-BEING-MADE* (FIND-SYSTEM-NAMED SYSTEM t t KEYWORDS))
	(SETQ *SYSTEM-DEFAULT-BINARY-FILE-TYPE*
	      (OR (GETF (SYSTEM-PLIST *SYSTEM-BEING-MADE*) 'DEFAULT-BINARY-FILE-TYPE)
		  (LOCAL-BINARY-FILE-TYPE)))
	(SETQ *TOP-LEVEL-TRANSFORMATIONS*
	      `(,@*LOAD-TYPE-TRANSFORMATIONS* DO-COMPONENTS-INTERNAL))
 
	;; Do all the keywords 			   
	(DO-THE-KEYWORDS KEYWORDS)
	(SETUP-FOR-OUTPUT-VERSION)
	;;If we are doing an old version (via :VERSION keyword), get all that setup
	(AND *USE-OLD-VERSION*
	     (DO-VERSION-KEYWORD))
	;; Make :NO-INCREMENT-PATCH override :COMPILE even if :COMPILE comes later.			   
	(WHEN *NO-INCREMENT-PATCH*
	  (SETQ *TOP-LEVEL-TRANSFORMATIONS*
		(DELETE-IF
		  #'(LAMBDA (X)
		      (MEMBER X '(INCREMENT-COMPILED-VERSION) :TEST #'EQ))
		  *TOP-LEVEL-TRANSFORMATIONS*)))
 
	;; If this is a patchable system, let's be sure the patch files are 
	;; around now instead of waiting for all the other transformations to
	;; finish before finding this out.  This isn't necessary, but it is a
	;; convience for the user to know of this situation early.
 
;        (AND (SYSTEM-PATCHABLE-P *SYSTEM-BEING-MADE*) ;all we care about is the side effect of
;             (PATCH-VERSION-NEWER-THAN-LOADED))       ;insuring the patch directories are out there
 
	;; Process forms with compiler context			   
	(DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-BEFORE*)
	  (eval form))
 
	(IF (FBOUNDP 'COMPILER:COMPILER-WARNINGS-CONTEXT-BIND)
	    (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
	       (PERFORM-TRANSFORMATIONS
		 (COLLECT-TOP-LEVEL-TRANSFORMATIONS *SYSTEM-BEING-MADE*)))
	    ;;Compiler isn't around.  Go without it.
	    (PERFORM-TRANSFORMATIONS
	      (COLLECT-TOP-LEVEL-TRANSFORMATIONS *SYSTEM-BEING-MADE*)))
 
	;; Finally process any forms queued by the keywords with compiler context			   
	(DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-AFTER*)
	  (eval form))
	;; See if any patches need to be loaded for this system.
	(WHEN (AND *LOAD-PATCHES*
		   (GET-PATCH-SYSTEM-NAMED *SYSTEM-BEING-MADE* T T)
		   (SYSTEM-PATCHABLE-P *SYSTEM-BEING-MADE*))
	  (LET ((LOAD-PATCHES-ARGS NIL))
	    (AND *SILENT-P* (PUSH :SILENT LOAD-PATCHES-ARGS))
	    (AND (EQ *QUERY-TYPE* :NOCONFIRM) (PUSH :NOCONFIRM LOAD-PATCHES-ARGS))
	    (APPLY #'LOAD-PATCHES :SYSTEMS (LIST (SYSTEM-SYMBOLIC-NAME *SYSTEM-BEING-MADE*)) LOAD-PATCHES-ARGS)))
	;;If :RECORD option was specified, do it.
	(AND *RECORD-VERSION-NUMBERS* (RECORD-SYSTEM-IN-LOG)))
 
      ;; Now forms outside of compiler context
      ;; These are done even if there was an error.			  
      (DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-FINALLY*)
	 (eval form)))
  *SOMETHING-LOADED*))
 
;;; No longer need to handle do-components specially
;;; here.  - ie, don't suck in those system's
;;; transformations

(DEFUN COLLECT-TOP-LEVEL-TRANSFORMATIONS (SYSTEM &OPTIONAL FORCE-DEPENDENCIES &AUX PKG)
  (SETQ SYSTEM (FIND-SYSTEM-NAMED SYSTEM) PKG (SYSTEM-PACKAGE-DEFAULT SYSTEM))
  (LET-IF PKG ((*FORCE-PACKAGE* PKG))
    (LOOP FOR XFORM IN (SYSTEM-TOP-LEVEL-TRANSFORMATIONS SYSTEM)
	  NCONC 
;                 (IF (EQ (TRANSFORMATION-TYPE-NAME (TRANSFORMATION-TRANSFORMATION-TYPE XFORM))
;			'DO-COMPONENTS-INTERNAL)
;		    (AND (MEMBER 'DO-COMPONENTS-INTERNAL *TOP-LEVEL-TRANSFORMATIONS* :TEST #'EQ)
;			 (LOOP FOR SUBSYS IN (SYSTEM-COMPONENT-SYSTEMS SYSTEM)
;			       WITH FORCE = (APPEND FORCE-DEPENDENCIES
;						    (TRANSFORMATION-DEPENDENCIES XFORM))
;			       NCONC (COLLECT-TOP-LEVEL-TRANSFORMATIONS SUBSYS FORCE)))
		    (CONS (LIST XFORM *FORCE-PACKAGE* FORCE-DEPENDENCIES) nil))))
 
;;; Previous definition of Perform-Transformation renamed
;;; to Perform-Transformations-Internal.  New definition
;;; just calls it multiple times.  Busts up the set of
;;; transformations into those preceding :do-components,
;;; and those after it and process each of those sets of
;;; transformations separately, and completely.  -dkm
;;; 11/87
 
(DEFUN PERFORM-TRANSFORMATIONS (TRANSFORMATION-LIST)
;;; Queue the transformations and pass the result onto the specified function
  ;; First do the work on any transformations which are inputs to these 
  (let* ((pos (position 'do-components-internal transformation-list
			    :key #'(lambda (transformation)
				     (transformation-type-name
				       (transformation-transformation-type (first transformation))))))
	 (first-xforms (firstn (or pos 0) transformation-list))
	 (last-xforms (nthcdr (or (and pos (1+ pos)) 0) transformation-list)))
    ;;; do all transformations preceding :do-components transformation, since that guy
    ;;; goes out and does complete make-systems on each specified component system.  11/87
    (and first-xforms
	 (perform-transformations-internal first-xforms))
    ;;; do the :do-components transformation all by itself (insures that it is done NOW. -dkm 11/87
    (and pos
	 (perform-transformations-internal (list (nth pos transformation-list))))
    ;;; now do the rest.  -dkm 11/87
    (and last-xforms
	 (perform-transformations-internal last-xforms))))
 
;;; This used to be PERFORM-TRANSFORMATIONS.  Nothing
;;; changed except his name (and recursive call to such).
;;; -dkm 11/87

(defun perform-transformations-internal (transformation-list)
  (LET ((INPUTS (LOOP FOR ELEM IN TRANSFORMATION-LIST
		      AS XFORM = (FIRST ELEM)
		      AND PKG = (SECOND ELEM)
		      AND FORCE = (THIRD ELEM)
		      AS INPUT = (TRANSFORMATION-INPUT XFORM)
		      WHEN (TYPEP INPUT 'TRANSFORMATION) COLLECT (LIST INPUT PKG FORCE))))
    (AND INPUTS (PERFORM-TRANSFORMATIONS-INTERNAL INPUTS)))	   ;change recursive call to new name  -dkm 11/87
  ;;Add files to *FILE-TRANSFORMATION-LIST*  
  (DOLIST (ELEM TRANSFORMATION-LIST)
    (LET ((*FORCE-PACKAGE* (SECOND ELEM))
	  (*SYSTEM-BEING-MADE* (TRANSFORMATION-SYSTEM (FIRST ELEM))))
      (QUEUE-ONE-TRANSFORMATION (FIRST ELEM) (THIRD ELEM))))
  (FUNCALL *FILE-TRANSFORMATION-FUNCTION*)) 
 
;;; minor change here so that the :do-components transformtation is actually executed
;;; so that the component-systems get :print-only make-systems done on them too. -dkm 11/87
 
(DEFUN PRINT-FILE-TRANSFORMATIONS ()
  "Implements the :PRINT-ONLY keyword of MAKE-SYSTEM.
This keyword causes MAKE-SYSTEM to print what it would do but not do it."
  (DOLIST (FILE-TRANSFORMATION *FILE-TRANSFORMATION-LIST*)
    (LET ((STATE (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION)))
      (CASE STATE
	((:DONE :REFUSED :NOT-NEEDED NIL))
	((:PENDING :PROBABLY)
	 (LET ((TYPE (FILE-TRANSFORMATION-TRANSFORMATION-TYPE FILE-TRANSFORMATION))
	       (ARGS (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION))
	       (OUTPUTS (FILE-TRANSFORMATION-OUTPUTS FILE-TRANSFORMATION))
	       (*FORCE-PACKAGE* (FILE-TRANSFORMATION-FORCE-PACKAGE FILE-TRANSFORMATION))
	       (*SYSTEM-BEING-MADE* (FILE-TRANSFORMATION-SYSTEM FILE-TRANSFORMATION)))
 
	   ;;; If we have a :do-components transformation, then process it so files from other
           ;;; the component systems can be reported too.  -dkm 11/87
	   (IF (EQ (TRANSFORMATION-TYPE-NAME TYPE) 'DO-COMPONENTS-INTERNAL)  ;is this the do-components xform?
	       (APPLY (TRANSFORMATION-TYPE-FUNCTION TYPE) ARGS)              ;if so, go run it   -dkm 11/87
	     (COND ((NOT *SILENT-P*)                                         ;else report it.
		    (IF (NULL (FILE-TRANSFORMATION-ARGS FILE-TRANSFORMATION))
			(FORMAT *QUERY-IO* "~&Need to ~\\FILE-XFORM-ARGS\\" FILE-TRANSFORMATION)
		      (FORMAT T
			      "~&~\\FILE-XFORM-ARGS\\~:[ probably then~] need~:[s~] to be ~A~
			         ~:[~; in~:[to~] package ~A~]"
			      FILE-TRANSFORMATION
			      (NEQ STATE :PROBABLY)
			      (NEQ (CDR ARGS) OUTPUTS)
			      (TRANSFORMATION-TYPE-PRETTY-PAST-PARTICIPLE TYPE)
			      *FORCE-PACKAGE*
			      (FILE-TRANSFORMATION-OUTPUTS FILE-TRANSFORMATION)
			      *FORCE-PACKAGE*)))))
	   (SETF (FILE-TRANSFORMATION-STATE FILE-TRANSFORMATION) :DONE)))
	(OTHERWISE
	 (FERROR nil "Transformation ~S in bad state" FILE-TRANSFORMATION))))))


;;;a CHANGE to these next two functions allows
;;;referencing modules external or component systems
;;;inside the current one.  can't say I think this is
;;;wise, but if you were dependent on some macros...

;;; Changed to call validate-external-module routine.  patch SYSTEM-3-30. -dkm 6/87

(DEFUN PARSE-MODULE-COMPONENTS (COMPONENTS SYSTEM)
  (COND ((PATHNAME-P COMPONENTS)	   ;a string or pathname object
	 (CONS (PARSE-MODULE-PATHNAME-LIST (CONS COMPONENTS NIL) SYSTEM) NIL))
	((SYMBOLP COMPONENTS)		   ;Single module within this system	
	 (LIST (FIND-MODULE-NAMED COMPONENTS SYSTEM)))	   ;return ((module-object))
	((not (listp COMPONENTS))
	 (FERROR nil "~S is not a recognized module component specification" COMPONENTS))
	((AND (SYMBOLP (CAR COMPONENTS))   ;list containing system followed by modules
	      ;if first symbol is a module, not external -dkm 6/87
	      (NOT (FIND-MODULE-NAMED (CAR COMPONENTS) SYSTEM T)))
	 (VALIDATE-EXTERNAL-MODULE COMPONENTS)	   ;parse external module spec is OK  -dkm 6/87

	 ;;this only occurs if the first element is a
	 ;;symbol specifying a component-system or other
	 ;;system. patch to return files, not a list.  TRP
	 ;;10/89.
	 (MAPCAR #'(lambda (name)
		     (FIND-MODULE-NAMED name (CAR COMPONENTS)))	;(CAR COMPONENTS) names a system
		 (CDR COMPONENTS)))
	(T
	 (PARSE-MODULE-COMPONENT-LIST COMPONENTS SYSTEM))))

;;; Changed to call new validate-external-module routine, and to support a list
;;; modules, as documented in LISP manual. patch SYSTEM-3-30. -dkm 6/87

(DEFUN PARSE-MODULE-COMPONENT-LIST (COMPONENTS SYSTEM)
  (LOOP FOR COMPONENT IN COMPONENTS
	COLLECT (COND ((PATHNAME-P COMPONENT)
		       (PARSE-MODULE-PATHNAME-LIST (CONS COMPONENT NIL) SYSTEM))
		      ((SYMBOLP COMPONENT)
		       (FIND-MODULE-NAMED COMPONENT SYSTEM))
		      ((not (listp COMPONENT))
		       (FERROR nil "~S is not a recognized module component specification" COMPONENT))
		      ((SYMBOLP (CAR COMPONENT))           
		       (COND ((FIND-MODULE-NAMED (CAR COMPONENT) SYSTEM T)  ;support list of modules  -dkm 6/87
			      ;;LOOP removed--ugly. TRP 10/89
			      (DOLIST (module component)
				(PUSH (find-module-named module system) modules)))
			     (T (VALIDATE-EXTERNAL-MODULE COMPONENT)   ;must be an external spec  -dkm 6/87
				;;changed to allow referencing component or external systems --TRP 10/89.
				(DOLIST (module (CDR component))
				  (PUSH (find-module-named module (CAR COMPONENT)) modules)))))
		      ((PATHNAME-P (CAR COMPONENT))
		       (PARSE-MODULE-PATHNAME-LIST COMPONENT SYSTEM))
		      (T (FERROR nil "~S is not a recognized module component specification" COMPONENT)))))


;;; ********** ********** ********** ********** ********** **********
;;; ********** ********** ********** ********** ********** **********
;;; ********** ********** ********** ********** ********** **********

;;; End Of File.

;;; ********** ********** ********** ********** ********** **********
;;; ********** ********** ********** ********** ********** **********
;;; ********** ********** ********** ********** ********** **********
