;;; -*- Mode:Lisp; Package:SYSTEM; Syntax:Common-Lisp; Patch-File: t -*-

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

;-------------------------------------------------------------------------------

;;; Modified TI source code from sys:make-system;defsys.lisp.
;;; changes marked by JPR.
;;;Edited by BUILD-CYC             17 Jan 90  14:29
(DEFUN FILE-NEWER-THAN-FILE-P (FILE-1 FILE-2 &OPTIONAL (NO-ERROR-P T))
    (COND ((EQ *SAFETY* 'VERSION)
	   ;; The new definition of this function uses versions if the OS supports them.  If not
	   ;; we automatically go and use creation dates.
	   (AND (EQUAL (SEND FILE-1 :NAME) (SEND FILE-2 :NAME))
		(LET ((FILE-1-VERSION (SYSTEM-GET-VERSION-NUM FILE-1))
		      (FILE-2-VERSION (SYSTEM-GET-VERSION-NUM FILE-2 NO-ERROR-P)))
		  (IF (= -2 (MIN FILE-1-VERSION FILE-2-VERSION)) ;version is -2 if versions not supported
		      (LET ((*SAFETY* 'CREATION-DATE))           ;bind *safety* to do dates and try again.
			(FILE-NEWER-THAN-FILE-P FILE-1 FILE-2 NO-ERROR-P))
		      (PROGN
			;;Now that the compiler can output files as :newest or :same version numbers,
			;;Issue a warning if there are versions of the binary greater than the version of the source.
			(IF (AND (< FILE-1-VERSION FILE-2-VERSION)
				 (EQ *OUTPUT-VERSION-BEHAVIOR* :SAME))
			    (FORMAT T "~%WARNING: ~A already has a larger version number than ~A. ~
                                       ~%         You probably need to use (:OUTPUT-VERSION :newest) in the DEFSYSTEM"
				    (SEND FILE-2 :NEW-PATHNAME :VERSION FILE-2-VERSION)
				    (SEND FILE-1 :NEW-PATHNAME :VERSION FILE-1-VERSION)))
			(> FILE-1-VERSION FILE-2-VERSION))))))

	((EQ *SAFETY* 'CREATION-DATE)
	 ;; The original definition of this function used creation dates for the selection criterion.
	 ;; This is now the alternate. 
	 (LET 
	   ((NEWER-DATE-P (> (SYSTEM-GET-FILE-PROPERTY FILE-1 :CREATION-DATE)	;file property 
			   (SYSTEM-GET-FILE-PROPERTY FILE-2 :CREATION-DATE NO-ERROR-P)))       
	    (NEWER-VERSION-P (AND (EQUAL (SEND FILE-1 :NAME) (SEND FILE-2 :NAME))	;not file property	       
				(> (SYSTEM-GET-VERSION-NUM FILE-1)
				   (SYSTEM-GET-VERSION-NUM FILE-2 NO-ERROR-P))))
	    (ZERO-LENGTH-P (EQl 0 (SYSTEM-GET-FILE-PROPERTY FILE-2 :LENGTH-IN-BYTES T))))	;file property
	   (ignore NEWER-VERSION-P)
	   (COND
	     (ZERO-LENGTH-P  T)			;ok if file doesn't exist yet
	     (T ;;;JPR commented out this bit.
	      NEWER-DATE-P))))
	(T (ERROR (FORMAT nil "Invalid *safety* value of ~a.  Must be either 'SI:VERSION or 'SI:CREATION-DATE" *safety*)))))

;-------------------------------------------------------------------------------


;;;Edited by RICE                  8 Jan 90  15:56
(defmacro (:property :system defsystem-macro) (name system-name &rest plist)
  (setf (getf (system-plist *system-being-defined*) :make-system-alist)
	(cons (list (string name) system-name)
	      (getf (system-plist *system-being-defined*) :make-system-alist)
	)
  )
  (let ((module (add-module name *system-being-defined* nil (copy-list plist))))
       (setf (module-plist module) (list :system system-name))
  )
  nil
) 

(DEFMACRO (:PROPERTY :DO-ALL-COMPONENTS DEFSYSTEM-MACRO) (&optional DEPENDENCIES)
  (SETQ *COMPONENTS-ALREADY-DONE* T)
  `(progn ,@(loop for dep in DEPENDENCIES collect `(DO-COMPONENTS-INTERNAL NIL ,dep)))
)

(advise parse-module-components :around :allow-nil-components nil
  (if (equal nil (third arglist))
      (catch-error :do-it nil)
      :do-it
  )
)


;;; Modified TI source code from sys:make-system;defsys.lisp.
;;; changes marked by JPR.
(DEFUN CALL-DEFSYSTEM-MACRO (FORM)
  (DO ((MACRO-FUNCTION) (VAL1) (VAL2))		;Kludge for multiple values       
      ((NULL FORM)
       (VALUES VAL1 VAL2))
    (SETQ MACRO-FUNCTION (GET (CAR FORM) 'DEFSYSTEM-MACRO))
    (OR (EQ (CAR MACRO-FUNCTION) 'MACRO)
	(eq (car form) 'progn)
	(FERROR nil "~S is not a valid DEFSYSTEM form" FORM))
    ;;; Change here by JPR.
    (if (eq (car form) 'progn)
       `(progn ,(loop for x in (rest form) collect (call-defsystem-macro x)))
	(MULTIPLE-VALUE-SETQ (FORM VAL1 VAL2)
	  (FUNCALL (CDR MACRO-FUNCTION) FORM)))))


;;;Edited by RICE                  8 Jan 90  15:56
(defmacro (:make-system defsystem-macro) (system-module-name)
  (let ((system-name (second (assoc (string system-module-name)
				    (getf (system-plist *system-being-defined*) :make-system-alist)
				    :test #'string-equal
			     )
		     )
	)
	(*add-transformation-to-system* T)
       )
       (putprop (locf (system-plist *system-being-defined*))
		system-name
		:make-system
       )
       (let ((input (find-if #'(lambda (module) (string= (string (module-name module)) (string system-module-name)))
			     (system-modules *system-being-defined*)
		    )
	     )
	    )
	    (if input
		(parse-transformation :make-system (module-name input) nil 'true)
		(ferror nil "Cannot find input module ~S for :make-system transformation." system-module-name)
	    )
       )
  )
    nil
)


(add-simple-transformation
  ':make-system 'Do-System-Dependencies nil nil
  '("Make component system" "Making component system" "component system made")
  T T
)

(defun do-system-dependencies ()
  (let ((module (fifth *file-transformation-being-made*)))
       (declare (special *file-transformation-being-made*))
       (apply #'make-system (getf (module-plist module) :system) *make-system-keywords*)
  )
)


;;; Modified TI source code from sys:make-system;defsys.lisp.
;;; changes marked by JPR.
;;;Edited by RICE                  5 Jan 90  22:36
(DEFUN GET-MODULE-PATHNAMES (MODULE &OPTIONAL (OTHER-SYSTEMS-OK T) TRANSFORMATION-INPUT-TYPE &AUX PKGPROP)
  (declare (special *transformation*))
  (or (LET-IF (SETQ PKGPROP (GETF (MODULE-PLIST MODULE) :PACKAGE))
	      ((*FORCE-PACKAGE* pkgprop))
	(GET-MODULE-COMPONENTS-PATHNAMES (MODULE-COMPONENTS MODULE) OTHER-SYSTEMS-OK TRANSFORMATION-INPUT-TYPE))
      ;;; Change here by JPR.
      (let ((entry (getf (module-plist module) :system)))
	   (if entry
	       (let ((FILE-TRANSFORMATION
			   (MAKE-FILE-TRANSFORMATION
			     :TRANSFORMATION-TYPE (TRANSFORMATION-TRANSFORMATION-TYPE *TRANSFORMATION*)
			     :FORCE-PACKAGE nil
			     :SYSTEM (TRANSFORMATION-SYSTEM *TRANSFORMATION*)
			     :module (transformation-input-module *transformation*) ;added dkm 02/88
			     :CONDITION-FUNCTION (TRANSFORMATION-CONDITION-FUNCTION *TRANSFORMATION*)
			     :OUTPUTS nil
			     :ARGS nil)))
		 (PUSH FILE-TRANSFORMATION *ADDED-FILE-TRANSFORMATIONS*)
		 (list (list :file-transformation FILE-TRANSFORMATION)))
	       nil))))


(advise queue-files-as-needed :around :fix-up-system-xforms nil
  (let ((result :do-it))
       (loop for xform in (first arglist)
	     when (and (consp xform) (consp (second xform)) (equal :make-system (first (second xform))))
	     do (setf (file-transformation-state xform) :probably)
       )
       result
  )
)

;;; Modified TI source code from sys:make-system;defsys.lisp.
;;; changes marked by JPR.
(DEFUN GET-TRANSFORMATION-INPUT-FILE-TRANSFORMATIONS (TRANSFORMATION)
;;; Get a list of FILE-TRANSFORMATION's from the INPUT to a single TRANSFORMATION
;; Will return a list for each input pathname consisting of the list: 
;;; (file-transformation object, transformation object, pathname)
  (LET ((INPUT (TRANSFORMATION-INPUT TRANSFORMATION))
	(TRANSFORMATION-TYPE (TRANSFORMATION-TYPE-TRANSFORMATION-INPUT-TYPE
			       (TRANSFORMATION-TRANSFORMATION-TYPE TRANSFORMATION)))
	(PATHNAME-LIST)
	(FILE-XFORM-LIST))
    (COND (INPUT
	   (CASE (type-of INPUT)
		 (TRANSFORMATION (SETQ PATHNAME-LIST (GET-TRANSFORMATION-PATHNAMES INPUT)))
		         ;;; Change here by JPR.
		 (MODULE (let ((*transformation* transformation))
			   (declare (special *transformation*))
			   (SETQ PATHNAME-LIST (GET-MODULE-PATHNAMES INPUT T TRANSFORMATION-TYPE))))
		 (OTHERWISE (FERROR nil "~S is not a valid transformation input" INPUT)))
	   (SETQ FILE-XFORM-LIST (LOOP FOR PATHNAME IN PATHNAME-LIST
				     COLLECT (if (and (consp pathname) (equal (first pathname) :file-transformation))
						 (second pathname)
						 (ADD-FILE-TRANSFORMATION TRANSFORMATION PATHNAME)))))
	  (T (SETQ FILE-XFORM-LIST (LIST (ADD-FILE-TRANSFORMATION TRANSFORMATION nil)))))
    FILE-XFORM-LIST))



;-------------------------------------------------------------------------------

;;; File written by JPR to simulate brand S defsystem features used by CYC
;;; on Explorers.


(defmethod process-defsubsystem-body-2 ((type (eql :serial)) &rest forms)
  (declare (special *modules* *transformations*))
  (multiple-value-bind (files modules transformations)
      (let ((*files* nil)
	    (*transformations* nil)
	    (*modules-in-this-form* nil)
	    (*transformation-type* :serial)
           )
	   (declare (special *files* *transformations* *modules-in-this-form*
			     *transformation-type*
		    )
	   )
	   (mapc 'process-defsubsystem-body-1 forms)
	   (values *files* *modules-in-this-form* *transformations*)
      )
    (let ((name (intern (gensym "SERIAL-MODULE-") 'user)))
         (gensym "G")
         (setq *modules* (union modules *modules* :test #'equalp))
	 (setq *transformations* (append transformations *transformations*))
	 (if files
	     (progn (push `(:module ,name ,(mapcar #'list (reverse files)))
			  *modules*
		    )
		    (push (apply 'make-transformation-for :module name
				 (mapcar 'make-fasload-form (mapcar #'second (reverse modules)))
			  )
			  *transformations*
		    )
	     )
	     (push `(:Do-Components
		      ,(post-transform-dependencies
			 (mapcar #'(lambda (x)
				     (make-fasload-form (if (symbolp x) x (second x)))
				   )
				   (reverse modules)
			 )
		       )
		    )
		   *transformations*
	     )
	 )
    )
  )
)



(defmethod process-type (name (type t) files &rest keyword-options)
  (ignore files keyword-options)
  (ferror nil "Unrecognised :module :type specifier ~S for module ~S" type name)
)

;;;Edited by RICE                  8 Jan 90  15:56
(defmethod process-type (name (type (eql :system)) system-name &rest keyword-options)
  (ignore keyword-options)
  (values (list :system name system-name)
	  (list :make-system name)
  )
)

(defmethod process-type (name (type (eql :lisp-example)) files &rest keyword-options)
  (apply 'process-type name :lisp files keyword-options)
)

(defmethod process-type (name (type (eql :font)) files &rest keyword-options)
  (apply 'process-type name :lisp files keyword-options)
)

(defmethod process-type (name (type (eql :lisp)) files &rest keyword-options)
  (ignore keyword-options)
  (values (cons :module (cons name (mapcar #'(lambda (file) (if (not (consp file)) (list file) file))
					   (zwei:list-if-not files)
			           )
	                )
	  )
	  `(:compile-load ,name)
  )
)


;;;Edited by RICE                  8 Jan 90  14:54
(defun make-fasload-form (module-name)
  (declare (special *modules* *modules-in-this-form*))
  (let ((module-name (if (consp module-name) (second module-name) module-name)))
       (let ((entry (or (find-if #'(lambda (x) (equal (second x) module-name)) *modules*)
	                (find-if #'(lambda (x) (equal (second x) module-name)) *modules-in-this-form*)
		    )
	     )
	    )
	    (if entry
		(make-fasload-form-1 module-name (first entry))
		(ferror nil "Module ~S not found." module-name)
	    )
       )
  )
)

(defmethod make-fasload-form-1 (name (type (eql :module)))
  `(:fasload ,name)
)

;;;Edited by RICE                  8 Jan 90  15:56
(defmethod make-fasload-form-1 (name (type (eql :system)))
  `(:make-system ,name)
)

;;;Edited by RICE                  8 Jan 90  15:49
(defun post-transform-dependencies (deps &optional (group-up-p t))
  (if group-up-p
      (labels ((dep1 (deps) (if (rest deps) (append (first deps) (list (dep1 (rest deps)))) (first deps))))
	(dep1 (reverse deps))
      )
      (labels ((dep2 (type deps)
		     (if deps
			 (if (equal type (first (first deps)))
			     (cons (second (first deps)) (dep2 type (rest deps)))
			     (append (first deps) (dep2 (first (first deps)) (rest deps)))
			 )
		     )
	       )
	      )
	(dep2 nil deps)
      )
  )
)

;;;Edited by RICE                  8 Jan 90  15:49
(defmethod make-transformation-for ((module-type (eql :module)) name &rest dependencies)
  `(:compile-load ,name ,(post-transform-dependencies dependencies nil))
)

(defmethod process-defsubsystem-body-2 ((type (eql :module)) &rest forms)
  (declare (special *modules* *transformations*))
  (let ((type (or (assoc :type (rest (rest forms))) '(:type :lisp))))
       (multiple-value-bind (mod trans)
	   (apply 'process-type (first forms) (second type) (second forms) (remove type (rest (rest forms))))
	 (pushnew mod *modules* :test #'equalp)
	 (pushnew trans *transformations* :test #'equalp)
       )
  )
)
       
(defmethod process-defsubsystem-body-2 ((type (eql 'quote)) &rest forms)
  (apply 'process-defsubsystem-body-2 (first forms))
)
       

;;;Edited by BUILD-CYC             17 Jan 90  14:56
(defmethod process-defsubsystem-body-2 ((type (eql :parallel)) &rest forms)
  (declare (special *modules* *modules-in-this-form* *transformations*))
  (multiple-value-bind (files modules transformations)
      (let ((*files* nil)
	    (*transformations* nil)
	    (*modules-in-this-form* nil)
	    (*transformation-type* :parallel)
           )
	   (declare (special *files* *transformations* *modules-in-this-form*
			     *transformation-type*
		    )
	   )
	   (mapc 'process-defsubsystem-body-1 forms)
	   (values *files* *modules-in-this-form* *transformations*)
      )
    (let ((name (intern (gensym "PARALLEL-MODULE-") 'user)))
         (gensym "G")
         (setq *modules* (union modules *modules* :test #'equalp))
	 (setq *transformations* (append transformations *transformations*))
	 (push `(:module ,name ,(mapcar #'list (reverse files))) *modules*)
	 (push (apply 'make-transformation-for :module name
		      (mapcar 'make-fasload-form (append (reverse *modules-in-this-form*) modules))
               )
	       *transformations*
	 )
    )
  )
)


;;;Edited by RICE                  8 Jan 90  14:54
;;;Edited by RICE                  8 Jan 90  15:49
(defmethod process-file ((type (eql :serial)) string)
  (declare (special *transformations* *modules-in-this-form*))
  (let ((name (intern (gensym (string-append "SERIAL-FILE-"
					     (string-upcase string)
					     "-"
			      )
		      )
		      *package*
	      )
	)
       )
       (gensym "G")
       (push `(:module ,name ((,string))) *modules-in-this-form*)
       (push `(:compile-load ,name ,(post-transform-dependencies
				     (mapcar #'(lambda (x)
						 (make-fasload-form (if (symbolp x) x (second x)))
					       )
					       (reverse (rest *modules-in-this-form*))
				     )
				     nil
				    )
	      )
	      *transformations*
       )
  )
)


(defmethod process-file ((type (eql :parallel)) string)
  (declare (special *files*))
  (push string *files*)
)


(defun find-module (name in-list)
  (find-if #'(lambda (x) (equal name (second x))) in-list)
)

(defmethod process-module-name ((type (eql :serial)) symbol)
  (declare (special *modules* *modules-in-this-form*))
  (or (find-module symbol *modules-in-this-form*)
      (let ((entry (find-module symbol *modules*)))
	   (if entry
	       (push entry *modules-in-this-form*)
	       (ferror nil "Module ~S not found." symbol)
	   )
	   entry
      )
  )
)


(defmethod process-module-name ((type (eql :parallel)) symbol)
  (declare (special *modules* *modules-in-this-form*))
  (or (find-module symbol *modules-in-this-form*)
      (let ((entry (find-module symbol *modules*)))
	   (if entry
	       (push entry *modules-in-this-form*)
	       (ferror nil "Module ~S not found." symbol)
	   )
	   entry
      )
  )
)


(defun process-defsubsystem-body-1 (form)
  (declare (special *transformation-type* *files* *modules-in-this-form* *transformations*))
  (etypecase form
    (cons (apply 'process-defsubsystem-body-2 form))
    (symbol (process-module-name *transformation-type* form))
    (string (process-file *transformation-type* form))
  )
  (values (if (boundp '*files*) *files* nil) *modules-in-this-form* *transformations*)
)


(defun process-defsubsystem-body (body)
  (let ((*modules* nil)
	(*modules-in-this-form* nil)
	(*transformations* nil)
       )
       (declare (special *modules* *modules-in-this-form* *transformations*))
       (loop for element in body do (process-defsubsystem-body-1 element))
       (append (reverse *modules*) (reverse *transformations*))
  )
)


(defmacro ticl:defsubsystem
	  (name (&key (documentation "") (pretty-name (symbol-name name)) (default-pathname nil))
	   &body body
	  )
  (defsubsystem-1 name pretty-name default-pathname documentation body)
)

(defun defsubsystem-1 (name pretty-name default-pathname documentation body)
 `(defsystem ,name
    (:pathname-default ,default-pathname)
    (:name ,pretty-name)
    (:documentation ,documentation)
    (:output-version :newest)
    ,@(process-defsubsystem-body body)
  )
)

(export 'ticl:defsubsystem 'ticl)

;-------------------------------------------------------------------------------

(defmacro ticl:define-system (name (&key (pretty-name nil)
				   (default-pathname nil)
				   (journal-directory nil)
				   (bug-reports nil)
                                   (documentation nil)
				   (distribute-sources nil)
				   (distribute-binaries nil)
				   (patches-reviewed nil)
				   (advertised-in nil)
				   (maintaining-sites nil)
			     )
			&rest modules-and-transformations
		       )
  (ignore journal-directory bug-reports distribute-sources distribute-binaries
	  patches-reviewed advertised-in maintaining-sites
  )
  (defsubsystem-1 name pretty-name documentation default-pathname modules-and-transformations)
)


(export 'ticl:define-system 'ticl)