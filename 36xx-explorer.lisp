;;; -*- Mode:Common-Lisp; Package:SYSTEM; Patch-file:T; Base:10; Fonts:(CPTFONT HL12 HL12I) -*-

;;; **********************************************************************
;;; Copyright (c) 1990 Stanford University.
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

;;; This software developed by:
;;;	James Rice
;;;	Rich Acuff
;;;	Nakul Saraiya
;;;	Sayuri Nishimura
;;; at the Stanford University Knowledge Systems Lab in 1985, 1986, and 1987.
;;;
;;; This work was supported in part by:
;;;	NIH Grant 5 P41 RR00785-15
;;;	DARPA Grant F30602-85-C-0012
;;;	DARPA Grant N00039-86-C-0033

;;; There is currently no TI restricted code in this file (19-Dec-87)

;;;  The purpose of this file is to aid in porting software from Symbolics 36xx
;;;  machines to Texas Instruments Explorers.  The incompatabilities are
;;;  enumerated below along with suggested workarounds, and, where possible,
;;;  code that implements similar behavior.  It is intended that this file be loaded on
;;;  the Explorer as part of systems being ported.  Many of the incompatibilities are
;;;  discussed in the TI document "Porting Software from Symbolcis 36xx to the
;;;  Texas Instruments Explorer", dated 11-Oct-85, including pathnames, Zmacs,
;;;  file system maintenance, and Common Lisp.

;;;  The comments herein refer to Explorer software Release 3.0 and
;;;  Symbolics Lisp release 6.1.

;;;  Any and all are welcome to use this code, under the provisions of TI licensing agreements.

;;;  Please send updates to Acuff@Sumex-AIM.ARPA.

;;;  Some extra compatibility can be obtained by using Common Lisp on both
;;;  machines.  The Explorer understands the 36xx's Attribute List Syntax for
;;;  Common Lisp.

;;;--------------------------------------------------------
;;;  The following are items which Explorer release 3 cause increased compatibility.

;;;  The Explorer can now interact with 36xx namespaces

;;;  The Explorer now has better floating point

;;;  SYS:DOWNWARD-FUNCTION and SYS:DOWNWARD-FUNARG declarations are now
;;;  supported on the Explorer.

;;;--------------------------------------------------------
;;;  The following are items for which there is currently no equivalent
;;;  functionality on the Explorer, and no easy work around.

;;;  Logical pathname syntax is slightly different.  On the Explorer it is
;;;  LHOST:D1.D2;NAME.TYPE#V, where the Symbolics uses
;;;  LHOST:D1;D2;NAME.TYPE.V

;;;  There is no PREEMPTABLE-READ on the Explorer, though there is
;;;  TV:PREEMPTABLE-READ-ANY-TYI-MIXIN.

;;;  There is no :MAINTAINING-SITE option to defsystem on the Explorer.

;;;  Different processes bind *PACKAGE*, causing PKG-GOTO to behave slightly
;;;  differntly on the two machines.

;;;  The TAPE and NETI packages do not exist on the Explorer, though much of the
;;;  functionality in them is present in the MT and NET packages.  CARRY format
;;;  tapes can be used to move data between 36xx's and Explorers that
;;;  aren't networked together.

;;;  This is in the TI compatibility document, but is worth mentioning again.
;;;  Zmacs is very different on the two machines, so any internal manipulation
;;;  of Zmacs data structures is likely to not port easily.

;;;  STRING-UPCASE and STRING-DOWNCASE take :START and :END arguments rather
;;;  than optional args.  Use CL:STRING-...

;;;  There appear to be no equivalents to the SI:CURSOR-X, SI:CURSOR-Y, and
;;;  SI:OUTPUT-STREAM instance variables of SI:BASIC-HARDCOPY-STREAM.  Of
;;;  course, the hardcopy systems are very different in other ways as well.

;;;  There is no SYS:READ-CHARACTER.  READ-CHAR can frenquently be used
;;;  instead.

;;;  SEND is a macro on the Explorer, and thus cannot be used with
;;;  apply.  Use FUNCALL instead of SEND with APPLY.

;;;  The Explorer does not support a :CHOOSE-MULTIPLE option for
;;;  TV:CHOOSE-VARIABLE-VALUES menus.

;;;  The PROMPT-AND-READ function does not support the :VISIBLE-DEFAULTS
;;;  option.

;;;--------------------------------------------------------
;;;  The following are things for which there is a non-automated
;;;  workaround.

;;;  By default, the Explorer puts compiler output into a file with the same version
;;;  number as the input source file.  Use COMPILER:*OUTPUT-VERSION-BEHAVIOR* to
;;;  change this.

;;;  ARRAY-DIMENSION-N is being obsoleted on the Explorer.  If you have
;;;  (ARRAY-DIMENSION-N dim array), then change it to (CL:ARRAY-DIMENSION
;;;  array (1- dim)), and it should work on both machines.

;;;  The right horseshoe character () is a delimiter on the Explorer and is used
;;;  with the print-readably mixin.  It is a symbol on the Symbolics.

;;;  Lambda expressions must always be function quoted (wrapped with function
;;;  or preceeded by #') on the Explorer.

;;;  LISTP is Common-Lisp compatible on the Explorer, but there is
;;;  a Zetalisp hang-on in Symbolics Lisp.  You can use CL:LISTP on
;;;  both.

;;;  Typecase syntax on the Explorer is Common-Lisp compatible, and so requires
;;;  OR, etc. in the clauses.

;;;  The :DEFAULT-INIT-PLIST keywords for TV:BORDERED-CONSTRAINT-FRAME
;;;  (and probably those like it) use :selection-substitute instead of
;;;  :selected-pane, and :CONSTRAINTS instead of :CONFIGURATIONS, and the
;;;  arguments for :CONSTRAINTS are different than those for :CONFIGURATIONS.

;;;  FS:COPYF of name.ext.n to name2.ext will try to write name2.ext#n on the
;;;  Explorer rather than name2.ext#>

;;;  Scrolling is very different on the two machines.

;;;  The function TV:SCROLL-MAINTAIN-LIST does not take the &REST
;;;  INIT-ARGS.  This can often be coded around by the use of a closure as
;;;  the Init-function.  However on Symbolics machines you are allowed to
;;;  supply NIL as the value of the intervening arguments, such as the
;;;  PRE-PROCESSING-FUNCTION and it assumes the correct default.  If you give
;;;  NIL to these arguments on the Explorer then it does not check this and all
;;;  sorts of nasty things happen to the window system when it tries to
;;;  redisplay the window.

;;;  In ZetaLisp mode, the argument order to CATCH and THROW are reversed.  The
;;;  easiest workarounds are to use CL: versions.

;;;--------------------------------------------------------
;;;  The following code adds functionality to the Explorer to make it more
;;;  compatible with Symbolics Lisp.

;;;  Add a COLOR package if there isn't one so the reader won't barf.
(unless (find-package "COLOR") (make-package "COLOR"))

;;;  Make the flavor inspector available
(tv:ADD-SYSTEM-KEY #\x 'tv:flavor-inspector "Flavor Inspector -- A utility for examining the structure of flavors.")

;;;  TV:ADD-SELECT-KEY (Symbolics) and TV:ADD-SYSTEM-KEY are similar, the
;;;  former having the CLOBBER-P arg.
TV:
(defun add-select-key (char-num window-or-flavor doc-string &Optional
		       (create-p t) clobber-p &Aux old-def)
   "   Symbolics style <Select> (<System>) key, with undocumented CLOBBER-P arg. If
   CLOBBER-P is not specified, ask user before putting new key on if there is
   already a different utility on that key."
   (setq old-def (assoc char-num *system-keys*))
   (if (and (not clobber-p)
	     old-def				; there is an old def'n
	     (or
	        (and				; it's the same
	           (equal (second old-def) window-or-flavor)
		 (equal (third old-def) doc-string)
		 (equal (fourth old-def) create-p))
	        (not (tv:mouse-y-or-n-p		; user wants to clobber it
		       (format nil "System-~C is already defined by ~A.  Redefine? "
			       char-num old-def)))))
         nil					; do nothing
         (add-system-key char-num window-or-flavor doc-string create-p)
         )
   )

;;;  Sundry functional interfaces to methods.
(defun Operation-Handled-p (an-object message)
  "Send an :OPERATION-HANDLED-P MESSAGE message to AN-OBJECT"
   (Send an-object :Operation-Handled-p message)
)
(export 'operation-handled-p "TICL")
(export 'operation-handled-p "GLOBAL")

(defun Send-if-Handles (to-object message &Rest other-arguments)
  "Send a :SEND-IF-HANDLES message to TO-OBJECT"
  (lexpr-send to-object :Send-if-Handles message other-arguments)
)
(export 'send-if-handles "TICL")

;;;  Easy entry into the debugger.
(unless (= 6 (sys:get-system-version))		;Don't if TI's version is around
    EH:
    (progn
      (compiler-let ((*standard-output* 'null-stream))
	;; Stop message being generated during the compilation of this function.
	(defsubst dbg ()
	  (compiler-let ((_dummy_ (if (and (boundp 'si:object-warnings-object-name)
					   si:object-warnings-object-name
						; Not during macroexpansion.
					   (boundp 'system:record-macros-expanded)
					   (not system:record-macros-expanded)
					   )
						; Generate a message when inside an fdefinition.
				      (compiler:warn :Debugger-Breakpoint
						     :Probable-Error
						     "Breakpoint in function ~S"
						     si:object-warnings-object-name
						     )
				      )
				  )
			 )
	    (cerror "Proceed from breakpoint" "Debugger breakpoint")
	    )
	  )
	)
      (multiple-value-bind (found? status)
	  (find-symbol "DBG" "IP")
	(when (and found? (neq status :inherited))
	  (unintern 'ip:dbg "IP")
	  )
	)
      (export 'eh:dbg "TICL")
      )
    )

;;;  Define some canonical file types.
(fs:define-canonical-type :directory "DIRECTORY")
(fs:define-canonical-type :bin "BIN")

;;;  Add the :PRETTY-NAME method for hosts.
NET:
(defmethod (basic-host :pretty-name) ()
  "Return the nicest looking name for self that is known."
  (send self :name)
  )

;;;  Add a few gray levels.
(defparameter tv:8%-gray (tv:make-gray 12. 3.
				       100
				       000
				       000
				       000
				       010
				       000
				       000
				       000
				       001
				       000
				       000
				       000))

(defparameter tv:7%-gray (tv:make-gray 14. 7.
				       1000000
				       0000000
				       0000100
				       0000000
				       0100000
				       0000000
				       0000010
				       0000000
				       0010000
				       0000000
				       0000001
				       0000000
				       0001000
				       0000000))

(defparameter tv:6%-gray (tv:make-gray 16. 8.
				       10000000
				       00000000
				       00010000
				       00000000
				       00000010
				       00000000
				       01000000
				       00000000
				       00001000
				       00000000
				       00000001
				       00000000
				       00100000
				       00000000
				       00000100
				       00000000))

;;;  Trivial float function.
(defun dfloat (a-number)
  "Coerce a-number into a double-float"
  (coerce a-number 'double-float))
(export 'dfloat "TICL")

;;;  Make up a READ-OR-END.  Note that this will not allow rubbing out leading
;;;  space after an expression is typed.  Ie., if you type "  (foo", only "(foo" can
;;;  be deleted.
(defun is-a-White-space-character (input-character)
    (let ((White-space-characters (List #\Space #\Tab       #\Return  #\Linefeed
			                #\Page  #\BackSpace #\Newline
			          )
	  )
         )
         (member input-character White-space-characters)
    )
)

(defun read-or-end (&Optional (stream *Standard-Input*) (reader #'Read))
    (read-or-end-2 stream reader 0)
)

(defun read-or-end-2 (stream reader depth)
    (let ((input-character (peek-char nil stream)))
	 (if (is-a-White-space-character input-character)
             (progn (princ (string-append (read-char stream)) stream)
		    (read-or-end-2 stream reader (+ 1 depth))
	     )
	     (if (equal #\Rubout input-character)
		 (if (> depth 0)
		     (progn (princ (string-append #\Backspace " " #\Backspace))
			    (read-char stream)
			    (read-or-end-2 stream reader (- depth 1))
		     )
		     (progn (read-char stream)
			    (read-or-end-2 stream reader 0)
		     )
		 )
	         (if (equal #\End input-character)
		     (Values nil :End)
		     (Values (Funcall Reader stream) nil)
		 )
	     )
	 )
    )
)
(export 'read-or-end "TICL")

;;;  Add a synonym.
(deff argument-typecase #'ctypecase)
(export 'argument-typecase "TICL")

;;;  Make BOUNDP-IN-INSTANCE.
(defun boundp-in-instance (instance variable)
  "True iff VARIABLE is bound in the INSTANCE."
  (sys:location-boundp (sys:locate-in-instance instance variable))
)
(export 'boundp-in-instance "TICL")

;;; Add this.
(defun si:get-release-version ()
  "Return info about the current release version: major number, minor
   number, and status"
  (let ((prod (car sys:*defined-products*)))
    (values (send prod :major-version) (send prod :minor-version))
    )
  )

;;;  Sundry mouse fns
(deff si:mouse-char-p 'tv:char-mouse-p)
(export 'si:mouse-char-p "TICL")

(defun si:char-mouse-button (char)
  "Returns the zero-origin number of the mouse button.  Left is 0."
  (ldb %%kbd-mouse-button char))
(export 'char-mouse-button "TICL")

(defun si:char-mouse-n-clicks (char)
  "Returns one less than the number of click in the mouse-char char."
  (ldb %%kbd-mouse-n-clicks char))
(export 'char-mouse-n-clicks "TICL")

;;;  Add this. (RDA)
(defmacro with-indentation ((stream indentation) &rest body)
  "Indent output to STREAM by INDENTATION for the execution of BODY."
  `(let ((,stream (si:make-stream-indentable ,stream)))
     (send ,stream :indent-relative ,indentation)
     (unwind-protect
	 (progn . ,body)
       (send ,stream :indent-relative (minus ,indentation))
       )
     )
  )

;;;  This is a close try for LETF. (RDA)
(defmacro letf (varlist &rest body &environment environ)
  "This is like let but uses the BIND primitive."
  `(let ((.values.))				;
     ,@(nreverse (loop for (var val) in varlist collect `(push ,val .values.)))
     ,@(loop for (var val) in varlist
	     for expanded = (macroexpand `(locf ,var) environ)
	     when (and (consp expanded) (eq 'funcall (first expanded))
		       (consp (second expanded))
		       (eq 'function (first (second expanded)))
		       (not (fdefinition-safe (second (second expanded))))
		  )
	     do (compiler:warn 'unknown-letf-locative :probable-error
			       "No known LOCF expander for the form ~S.  ~
                                You probably cannot use LETF with this accessor form." var)
	     collecting `(bind (locf ,var) (pop .values.)))
     . ,body)
  )
;;SPR 149: Don't get blocked by ZWEI:LETF being present
(multiple-value-bind (found? status)
    (find-symbol "LETF" "ZWEI")
  (when (and found? (neq status :inherited))
    (unintern 'zwei:letf "ZWEI")
    )
  )
(export 'letf "TICL")

;;;  TI seems to have dropped this. (RDA)
(defun format-symbol (ctl-string &rest args)
  "Like (MAKE-SYMBOL (FORMAT NIL CTL-STRING ...) T), but recycles the string's storage."
  (let ((string (nstring-upcase (apply #'format nil ctl-string args) :start 0)))
    (prog1 (make-symbol string t)
	   (return-array string))))

;;;  Add the :DECIMAL-NUMBER choice type for CHOOSE-VARIABLE-VALUES menus.
(defun choose-variable-values-decimal-number-read (stream)
  "Read in and validate a deciaml number."
  (let* ((*read-base* 10.)
	 (*print-base* 10.)
	 (input (read stream)))
    (or (numberp input)
	(ferror nil "A number is required"))
    input))
TV:
(defun prin1-decimal (value stream)
  "Print VALUE with PRIN1,  but in base 10."
  (let ((*print-base* 10.))
    (prin1 value stream)
    )
  )

(defprop :decimal-number
	 (tv:prin1-decimal tv:choose-variable-values-decimal-number-read nil nil nil
		"Click left to enter a decimal number from the keyboard")
	 tv:choose-variable-values-keyword)

;;;  Add this
(defflavor tv:truncatable-lines-mixin () (tv:line-truncating-mixin))

(defun tv:funcall-with-swapped-blinkers
  (window body-function &rest body-function-args)
"Switches off the blinkers of window during the execution of the body function
 and swiches on the typeout window's blinkers.  Cleans up afterwards.
"
  (declare (unspecial window body-function body-function-args))
  ;;;Defined by JPR on n19 Mar 87 for typeout windows.
  (flet ((message-to-all-blinkers (a-window message &Rest args)
	    (Apply #'mapcar
		   #'(lambda (blinker &rest other-args)
		       (lexpr-send blinker message other-args)
		     )
		     (send a-window :blinker-list) args
	    )
	 )
	)
        (let ((typeout-window (send window :typeout-window))
	      (old-visibilities
		(message-to-all-blinkers window :visibility)
	      )
	      (new-visibilities
		(message-to-all-blinkers window :deselected-visibility)
	      )
	     )
	     (declare (unspecial typeout-window
				 superior-visibilities
				 new-visibilities
		      )
	     )
	     (unwind-protect
		 (progn (tv:turn-on-sheet-blinkers typeout-window)
			(message-to-all-blinkers window :Set-visibility
						 new-visibilities
			)
			(apply body-function body-function-args)
		 )
	       (message-to-all-blinkers window :Set-visibility old-visibilities)
	       (tv:turn-off-sheet-blinkers typeout-window)
	     )
	)
  )
)


;;;  Add WITH-TERMINAL-IO-ON-TYPEOUT-WINDOW.
TV:
(defmacro with-terminal-io-on-typeout-window
	  ((window wait-for-char-p) &Body body)
  "Execute BODY with *TERMINAL-IO* bound to WINDOW's typeout window,
   taking care of exposure and deexposure of WINDOW.  If
   WAIT-FOR-CHAR-P is non-NIL and there is output, don't return until a
   character is typed."
  `(let* ((.t-window. (send ,window :typeout-window))
	  (*terminal-io* .t-window.))
     (send .t-window. :make-complete)
     (funcall-with-swapped-blinkers ,window
      #'(lambda ()
	  ;;Selection substitute put in by JPR 19 Mar 87.

	    (unwind-protect
	      (tv:with-selection-substitute (.t-window. ,window)
		(multiple-value-prog1
		   ;; Do whatever it is
		  (progn (send .t-window. :expose-for-typeout)
			 ,@body)
		   ;; Wait if we're supposed to
		  (when (and ,wait-for-char-p
			     (send .t-window. :incomplete-p))
		    (format t "~&Type a space to clear this output. ")
		    (tyi .t-window.)
		    )
		  )
		)
	      (send .t-window. :make-complete)
	      (send .t-window. :DeExpose)
	      (when (send .t-window. :active-p)
		(send .t-window. :deactivate))
	      )
	  )
      )
     )
  )

;;;  Add temporary-typeout-window
TV:
(defflavor temporary-typeout-window-mixin
	   ((bits-covered? nil)
	    (covered-bits nil))
	   ()
  (:required-flavors basic-typeout-window)
  ;; The put in by JPR on 19 Mar 87.
  (:default-init-plist :Blinker-p nil)
  )


TV:
(defmethod (temporary-typeout-window-mixin :before :expose-for-typeout)
	   (&rest ignore)
  "Save the bits of our superior"
  (let ((super-width (send superior :width))
	(super-height (send superior :height)))
    (if covered-bits			;been created yet?
	;; yes, then make sure it's big enough
	(let ((save-height (zlc:pixel-array-height covered-bits))
	      (save-width (zlc:pixel-array-width covered-bits)))
	  (when (or (< save-height super-height) ;need to grow?
		    (< save-width super-width))
	    (tv:grow-bit-array covered-bits super-width
			       super-height super-width
			       save-height save-width nil)
	    )
	  )
	;; nothing yet, make one
	(setf covered-bits (make-sheet-bit-array self
				super-width super-height))
	)
      ;; Save the old stuff
    (bitblt alu-seta super-width super-height
	    (sheet-screen-array superior) 0 0
	    covered-bits 0 0)
      ;; Remember we've saved something
    (setf bits-covered? t)
    )
  )

TV:
(defmethod (temporary-typeout-window-mixin :after :deexpose) (&rest ignore)
  "Restore the bits of our superior."
  (when (and bits-covered?
	     (not (send self :incomplete-p)))
    (sheet-force-access (superior t)
      (bitblt alu-seta (send superior :width)
	      (send superior :height)
	      covered-bits 0 0
	      (sheet-screen-array superior) 0 0))
    (setf bits-covered? nil)
    )
  )

TV:
(defflavor temporary-typeout-window
	   ()
	   (temporary-typeout-window-mixin typeout-window))

;;;  Make semicolons on the ends of logical directories be allowed on Explorers.
;;;  Note that this does NOT address the use of multiple semicolons in a name (eg.
;;;  DOC;MENUS;) which would still need to be written DOC.MENUS; on the Explorer.
NET:
(defun PARSE-LOGICAL-TRANSLATIONS (physical-host translations)
  ;; Format of the translations can be: (logical-directory physical-device-and-directory)
  ;;                                    (logical-directory physical-device physical-directory)
  ;; (Suggested Format)                 (logical-directory physical-device (subdir subdir ...))

 (if (not (listp (car translations)))
     ;; Should be a function for (:METHOD HOST :DIRECTORY-TRANSLATIONS) to call.
     translations
  (let* ((physical-host (get-host physical-host))
	 defdev)

    (values
      (loop for translation in translations
	    with logical-directory and physical-device and physical-directory

	    do (cond ((consp (third translation))
		      (setf (list logical-directory physical-device physical-directory)
			    translation))
		     ((stringp (third translation))

		      (setf logical-directory (first translation))

		      (setf physical-device (send (fs:sample-pathname physical-host)
						  :parse-device-spec (second translation)))

		      (setf (values nil physical-directory)
			    (send (fs:sample-pathname physical-host)
				  :parse-namestring nil (third translation)))

		      )

		     (t
		      (setf (list logical-directory physical-directory) translation)

		      (setf (values physical-device physical-directory)
			    (send (fs:sample-pathname physical-host) :parse-namestring nil
				  physical-directory))))

	    when (and logical-directory
		      (member physical-directory '(nil :unspecific) :test #'eq))
	    do (ferror nil "No directory specified in ~A, you probably forgot some delimiter characters."
		       translation)

	    ;; A translation for logical directory NIL specifies the default device.
	    when (null logical-directory)
	    do (setq defdev physical-device)

	    when logical-directory
	    collect (fs:make-logical-pathname-translation
		      ;; Ensure that the logical directory name is uppercase
		      ;;RDA: Add STRING-TRIM to allow ";" on the end of logical dirs, and change to Nstring-upcase
		      :logical-directory (nstring-upcase (string-trim ";" logical-directory))
		      :physical-device physical-device
		      :physical-directory physical-directory
		      ))
      defdev))))


;-------------------------------------------------------------------------------

;;; G7 has this one.

(defmacro ticl:with-standard-io-environment (&body body)
 `(let ((*package* (find-package 'user))
        (*read-base* 10.)
	(*print-base* 10.)
	(*nopoint nil)
	(*readtable* sys:common-lisp-readtable)
	(*print-case* :Upcase)
	(*print-structure* t)
	(*print-circle* nil)
	(*print-level* nil)
	(*print-length* nil)
	(sys:*lisp-mode* :Common-Lisp)
	(sys:*reader-symbol-substitutions*
	  sys:*common-lisp-symbol-substitutions*
	)
       )
       ,@body
  )
)


(export 'ticl:with-standard-io-environment 'ticl)

;;; 01-Feb-91 RDA: Make the Explorer able to read Symbolics file attribute lists with :COLON-MODE

(advise (:property :package fs:file-attribute-bindings)
	:before 'no-colon-mode nil
  (let ((name (third arglist)))
    (if (and (consp name) (member :colon-mode name :test #'eq))
	(loop for l on name
	      do (when (eq (second l) :colon-mode)
		   (setf (rest l) (cdddr l))))))
  )
