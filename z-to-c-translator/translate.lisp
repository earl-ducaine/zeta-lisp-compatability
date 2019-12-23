
(in-package :transl)


(export '(translate-file translate-system))

(proclaim '(optimize (safety 0) (speed 3)))

(defmacro defprop (symbol value property)
  "make the value of symbol's property property be value."
  `(progn
     (setf (get ',symbol ',property) ',value)
     ',symbol))

(defvar *hook-hash* (make-hash-table :size 30 :test #'eq))
(defvar tr-read (copy-readtable (copy-readtable nil)))
(defvar tr-cl-read (copy-readtable (copy-readtable nil)))
(defparameter cli-package (find-package "CLI"))

(defprop tr-comm grind-comment si::grind-macro)

;; phd 4/15/86 added support for backquote translation
(defvar  *translate-backquote* nil)

(use-package 'zwei)

(defmacro warning (text body)
  (compiler:warn 'translation-warning :ignorable-mistake text)
  body)

;; The change in this function skips over blank and comment lines
;; before starting to read the next s-expression.
(defun stream-out-next-expression ()
  "Return a stream for the next s-expression and advance point"
  (prog1 (zwei::interval-stream (zwei:skip-over-blank-lines-and-comments (zwei::point))
			  (zwei::interval-last-bp zwei::*interval*) t t)
	 (zwei::com-forward-sexp)))

;; The change-occured check avoids all the grinding lossage if there
;; are no changes.  The with-undo-save means that one can answer Y and
;; still go back to the original with a single UNDO.
(zwei:defcom com-translate-next-sexp  "" ()
 (let((context nil) (trace nil) (change-occured nil)
        (*translate-backquote* nil))
  ;;(declare (special context trace change-occured))
  (clrhash *hook-hash*)
  (let* ((*print-array* t) ;phd 3/15/86 #(1 2 3) will be printed correctly
	 (stream (stream-out-next-expression))
	 (form (let ((*readtable* (if (zetalisp-on-p )
				      tr-read
				      tr-cl-read))
		     (si:READ-DISCARD-FONT-CHANGES nil ))
		 (read  stream nil '*EOF* nil nil t))))
    (if (or (null form)
	    (EQ FORM '*EOF*))
	(zwei::BARF "there is no form to translate ; position the cursor at the beginning of a form")
	(progn
	  (translate-form form nil)
	  (transpose-package form)
	  (cond ((null change-occured)
		 (format *query-io* "~&No changes necessary."))
		(t
		 (splice-comment form)
		 (zwei:with-undo-save ("Translation" (zwei:forward-sexp (zwei:point) -1) (zwei:point) t)
		   (let ((*readtable* #.*readtable*)
			 (si:*lisp-mode* :common-lisp)
			 (si:*READER-SYMBOL-SUBSTITUTIONS* si:*COMMON-LISP-SYMBOL-SUBSTITUTIONS* ))
		     (pprint form  (zwei::INTERVAL-STREAM-INTO-BP (ZWei::point) t)))
		   (zwei::must-redisplay zwei::*window* zwei::dis-text)
		   (zwei::redisplay zwei::*window* )
		   (if (y-or-n-p "Is this ok?")
		       (progn
			 (zwei::com-backward-kill-sexp)
			 (zwei::com-kill-line)
			 (zwei::com-forward-sexp))
		       (zwei::com-kill-sexp))))))))
  zwei::dis-text))
(zwei:defcom com-translate-next-sexp-including-backquotes  "" ()
 (let((context nil) (trace nil) (change-occured nil)
      (*translate-backquote* t))
  (declare (special context trace change-occured))
  (clrhash *hook-hash*)
  (let* ((*print-array* t) ;phd 3/15/86 #(1 2 3) will be printed correctly
	 (stream (stream-out-next-expression))
	 (form (let ((*readtable* (if (zetalisp-on-p )
				      tr-read
				      tr-cl-read))
		     (si:READ-DISCARD-FONT-CHANGES nil ))
		 (read  stream nil '*EOF* nil nil t))))
    (if (or (null form)
	    (EQ FORM '*EOF*))
	(zwei::BARF "there is no form to translate ; position the cursor at the beginning of a form")
	(progn
	  (translate-form form nil)
	  (transpose-package form)
	  (cond ((null change-occured)
		 (format *query-io* "~&No changes necessary."))
		(t
		 (splice-comment form)
		 (zwei:with-undo-save ("Translation" (zwei:forward-sexp (zwei:point) -1) (zwei:point) t)
		   (let ((*readtable* #.*readtable*)
			 (si:*lisp-mode* :common-lisp)
			 (si:*READER-SYMBOL-SUBSTITUTIONS* si:*COMMON-LISP-SYMBOL-SUBSTITUTIONS* ))
		     (pprint form  (zwei::INTERVAL-STREAM-INTO-BP (ZWei::point) t)))
		   (zwei::must-redisplay zwei::*window* zwei::dis-text)
		   (zwei::redisplay zwei::*window* )
		   (if (y-or-n-p "Is this ok?")
		       (progn
			 (zwei::com-backward-kill-sexp)
			 (zwei::com-kill-line)
			 (zwei::com-forward-sexp))
		       (zwei::com-kill-sexp))))))))
  zwei::dis-text))

; Hyper-T is bound to the translation command
(zwei::set-comtab zwei::*standard-comtab* `( #\hyper-T com-translate-next-sexp))
(zwei::set-comtab zwei::*standard-comtab* `( #\hyper-super-T com-translate-next-sexp-including-backquotes))

(zwei:defcom com-install-translator-key
	     "Install the Hyper-T key in the current comtab" ()
  (zwei::set-comtab zwei::*comtab* `(#\hyper-T com-translate-next-sexp))
  (zwei::set-comtab zwei::*comtab* `(#\hyper-super-T com-translate-next-sexp-including-backquotes))
  zwei::dis-none)

(si:defprint transl:tr-comm (si:pprint-handler si:pp-objify-comment))

;; 12/08/88 DNG - Add writing of mode line.  Add CATCH-ERROR-RESTART.
;; 12/13/88 DNG - Bind *PRINT-STRUCTURE* true.
;; 12/21/88 DNG - Ensure that *PRINT-LENGTH* and *PRINT-LEVEL* are NIL.
;;  1/05/89 DNG - Return the truename.
;;  1/14/89 DNG - Add CASE argument.
(defun translate-file (ifile ofile &optional trace new-base (case *print-case*)
		       &aux (eof (cons nil nil)) (cl-readtable #.*readtable* ))
  "Translates IFILE from Zetalisp to OFILE Common Lisp.
   TRACE set to T will cause all changes to be printed out on *STANDARD-OUTPUT*.
   NEW-BASE indicates the print base of the translator, nil means use the file's base.
   CASE should be either :UPCASE or :DOWNCASE."
  (declare (special trace))
  (SETQ IFILE (FS:MERGE-PATHNAME-DEFAULTS IFILE :LISP))
  (SETQ OFILE (FS:MERGE-PATHNAME-DEFAULTS OFILE IFILE))
  (clrhash *hook-hash*)
  (with-open-file (is ifile :direction :input)
    (with-open-file (os ofile :direction :output :if-exists :new-version)
      (let ((generic-pathname  (FUNCALL IFILE ':GENERIC-PATHNAME)))
	(FS:READ-ATTRIBUTE-LIST GENERIC-PATHNAME IS)
	(MULTIPLE-VALUE-BIND (VARS VALS) (FS:FILE-ATTRIBUTE-BINDINGS GENERIC-PATHNAME)
	  (PROGV VARS VALS
	    (let ((si:READ-DISCARD-FONT-CHANGES nil )
		  (*translate-backquote* nil)
		  (context nil)
		  (*print-base* (or new-base *print-base*)))
	      (declare (special context))
	      (unless (and (sys:common-lisp-on-p)
			   (eql *read-base* *print-base*))
		(format os ";;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ~A; Base: ~D -*-~%"
			(package-name *package*) *print-base*))
	      (loop
		(if (char= (let ((char (peek-char nil is nil eof)))
			     (if (eq char eof )
				 (return nil)
				 char)) #\( )
		    (let (exp (change-occured nil))
		      (declare (special change-occured))
		      (let* ((*readtable* (if (zetalisp-on-p )
					      tr-read
					    tr-cl-read)))
			(setf exp (read is nil EOF nil nil t))
			(when (eq exp eof )
			  (return nil)))
		      (catch-error-restart ((error break)
					    "Leave this form untranslated and proceed with the next top-level form.")
			(translate-form exp nil))
		      (transpose-package exp)
		      (splice-comment exp)
		      (let ((*readtable* cl-readtable)
			    (si:*READER-SYMBOL-SUBSTITUTIONS* si:*COMMON-LISP-SYMBOL-SUBSTITUTIONS*)
			    (*print-array* t)		  ;phd 3/15/86 #(1 2 3) will be printed correctly
			    (*print-structure* t)
			    (*print-length* nil)
			    (*print-level* nil)
			    (*print-case* case))
			(pprint  exp os)))
		    (write-line (let ((line (read-line is nil eof)))
				  (if (eq line  eof)
				      (return nil)
				      line))
				os)))))))
      (truename os))))

;; 12/08/88 DNG - Original version.
;;  1/05/89 DNG - Avoid possibility of doing the defsystem file twice.
;;  1/14/89 DNG - Use keyword arguments.
(defun translate-system (system-name output-directory
			 &key (trace nil) base (case *print-case*) (radix *print-radix*))
  "Translates all of the files in a system from Zetalisp to Common Lisp.
The first argument is the name of the system, and the second is the pathname
of the directory where the translated files are to be written.
TRACE set to T will cause all changes to be printed out on *STANDARD-OUTPUT*
BASE indicates the print base of the translator, NIL means use the file's base.
CASE is the value for *PRINT-CASE* -- either :UPCASE or :DOWNCASE.
RADIX is the value for *PRINT-RADIX* -- true to include explicit radix on numbers."
  (let* ((files (sys:system-files system-name '(:recompile :do-not-do-components)
				  '(:compile :readfile)))
	 (system (sys:find-system-named system-name t t))
	 (ofiles '()))
    (unless (null system)
      (pushnew (sys:merge-pathname-type (sys:get-source-file-name (sys:system-symbolic-name system)
								  'DEFSYSTEM)
					:LISP)
	       files :test #'eq))
    (dolist (file files)
      (catch-error-restart ((error break)
			    "Give up translating file \"~A\" and continue with the next one." file)
	(when trace
	  (format t "~2%  ===  Translating file ~A  ===~%" file))
	(push (let ((*print-radix* radix))
		(translate-file file output-directory trace base case))
	      ofiles)))
    (nreverse ofiles)))

(defun find-incompatible-symbol (exp)
  (declare (special change-occured))
  (do ((sexp exp (cdr sexp) ))
      ((atom sexp))
    (if (symbolp (car sexp))
	(when (rASSOC (CAR sexp)
		     SI:*zetalisp-SYMBOL-SUBSTITUTIONS*
		     :TEST #'EQ)
	  (setf change-occured t)
	  (return))
	(find-incompatible-symbol (car sexp)))))

;; It seems the right thing to do is to always return true, i.e. not a
;; macro compiled program
(defmacro check-for-fef-area (object)
  t)
  ;; `(/= macro-compiled-program
  ;;      (sys:%area-number ,object)))

(defun transpose-package (exp &aux new-sym)
  ;; 2/18/86 added a check for fef area.
  (declare (special change-occured))
  (typecase exp
    (atom)
    (cons (cond
	    ((eq 'quote (car exp))
	     (find-incompatible-symbol exp))
	    (t(do ((sexp exp (cdr sexp) ))
		  ((atom sexp))
		(if (symbolp (car sexp))
		    (when  (and (setf new-sym
				 (car (rASSOC (CAR sexp)
					     SI:*zetalisp-SYMBOL-SUBSTITUTIONS*
					     :TEST #'EQ)))
				(check-for-fef-area sexp))
		      (setf (car sexp) new-sym ))
		    (transpose-package (car sexp)))
		(if (symbolp (cdr sexp))
		    (when  (and (setf new-sym
				 (car (rASSOC (cdr sexp)
					     SI:*zetalisp-SYMBOL-SUBSTITUTIONS*
					     :TEST #'EQ)))
				(check-for-fef-area sexp))
		      (setf (cdr sexp) new-sym )))))))
    (otherwise nil)))

(defun min-nsubst-eq (new old s-exp)
  ;;like nsubst but change the s-exp only when it needs to.
  (cond ((eql old s-exp) new)
	((atom s-exp) s-exp)
	(t (do ((s s-exp (cdr s))
		(prev nil s) new-car)
	       ((atom s)
		(when (and (eq old s)
			   (check-for-fef-area prev))
		  (setf (cdr prev) new)))
	     (if (atom (car s))
		 (when (eq old (car s))
		   (setf (car s) new))
		 (setf new-car (min-nsubst-eq new old (car s)))
		 (when (and (neq new-car (car s))
			    (check-for-fef-area s))
		   (setf (car s) new-car))))
	   s-exp)))

(defun trace-change (form exp)
  (declare (special trace context change-occured))
  (setf change-occured t)
  (when (and (boundp 'trace) trace context)
    (let ((*print-length* 3)
	  (*print-level* 2)
	  (*readtable* #.*readtable*))
      (WITH-COMMON-LISP-ON
	(format *standard-output* "~%In ~S~%	 ~S => ~S" (car (last context)) form exp)))))


;; Used to dynamically
(defmacro change (form exp)
  "change the contents of the list preserving the first cons cell"
  ;; phd 2/14/86 Added a check for fef area, in case we trnaslate
  ;; something coming from a macro expansion.
  `(when (check-for-fef-area ,(if(atom form) form (second form)))
     ,(if (atom form)
	  ;the form is the whole list
	  `(progn (trace-change ,form ,exp)
		  (change-body ,form ,exp) nil)
	  `(progn (trace-change ,form ,exp)
		  (setf ,form ,exp) nil))))


(defmacro change-body (f exp)
  `(let ((tmp ((lambda (,f),f ,exp) ,f)))
       (psetf (car ,f) (car tmp)
	      (cdr ,f) (cdr tmp))))

(defun translate-form (form context )
  (declare (special context))
  (if (null (translate form))
      (progn
	(push form context)
	(do ((sub-forms (cdr form) (if (consp sub-forms ) (cdr sub-forms) nil)))
	    ((null sub-forms))
	  (translate-form (if (consp sub-forms) (car sub-forms) sub-forms) context)))))

(defun nlam ()
  t)

(defun dothis (form parent-form)
  (declare (special context))
  (translate-form form (cons parent-form context))
  t)

(defun dothese (forms parent-form)
  (declare (special context))
  (let ((context context))
    (declare (special context))
    (push parent-form context)
    (when (consp forms)
      (do ((sub-forms forms (if (consp sub-forms ) (cdr sub-forms) nil)))
	  ((null sub-forms))
	(translate-form
	 (if (consp sub-forms)
	     (car sub-forms)
	     sub-forms)
	 context)))
    t))

(defun translate (form &aux transform macro-function expansion )
  ;;phd 2/14/86 Added special handling for macros
  (declare (special context change-occured))
  (cond ((atom form)t)
	((setf transform
	       (and (consp form)
		    (SYMBOLP (CAR form))
		    ;(neq (symbol-package (car form)) cli-package) ; do not translate cli symbols
		    (get (or (cdr (ASSOC (CAR form)
					 SI:*COMMON-LISP-SYMBOL-SUBSTITUTIONS*
					 :TEST #'EQ))
			     (car form))
			 'transform)))
	 (funcall transform form))
	((setf macro-function
	       (and (consp form)
		    (SYMBOLP (CAR form))
		    (macro-function (car form))))
	 (unless (equal (setf expansion (funcall macro-function form))
			form)
	   ;; PHD 2/24 Try to set the change-occured flag only if the macro call has been changed
	   ;; (ignoring any spurious translation due to the macroexpansion)
	   (unless
	     (let ((original-form (copy-tree form))
		   (change-occured nil)) ;; Bind it So we save its previous value
	       (declare (special change-occured))
	       (translate-form expansion context)
	       (equal form original-form))
	     (setf change-occured t)))
	 ;; do not do the subforms
	 t)))

;;macro used to define new translations forms
(defmacro deftranslation (name parm-list &body body)
  (if (and (not (listp parm-list))
	   (null body))
      `(defprop ,name ,parm-list transform)
    `(defun (:property ,name transform)
	    ,parm-list ,@body)))

;; Macro used to define a translation that will only replace the car
;; of the form by the newname
;; (defmacro defreplace (name newname)
;;   `(defun (:property ,name transform)
;; 	  (form)
;;      (change (first form) ',newname)))

;; Instead of using the (also) obsolete :property dispatch mechanism
;; of function definition, simply create a translating macro. This
;; works well for us because we don't even have the original
;; definition.
;; (defmacro defreplace (name newname)
;;   (emit-defreplace-macro name newname))

(defmacro defreplace (name newname)
  `(defmacro ,name (&body body)
     `(,',newname ,@body)))

;; (defun emit-defreplace (name newname)
;;   (let ((form
;; 	 '(defmacro name (&body body)
;; 	   `(newname ,@body))))
;;     (setf (cadr form) name)
;;     (setf (car (cadr (cadddr form))) newname)
;;     form))

;; (defun emit-defreplace-macro (name newname)
;;   `(defmacro ,name (&body body)
;;      `(,newname ,@body)))


;; (defmacro p-defgeneric-options (&body body)
;;   `(defgeneric ,@body))


(p-defgeneric-options
 add-method
 ((generic-function
   (if (typep generic-function 'generic-function)
       generic-function
       (get-generic-function-object generic-function)))
  method)
 (:documentation "Add a method to a generic function.
The generic function is destructively modified and returned as the result."))




;; This function tries to hook the comments to the form that precedes them
;; in the context of a top level form
;; (gethash top-level-form *hook-hash* ) is an alist which key is the element number of the form in the top-level form
;; and contents is a list of comments that were after that form
;; The problem is if the top-level-form begins with a comment, that one is hooked to nil instead of the top-level-form
(defun read-comment ( st char)
  (declare (ignore char))
  (let ((string (read-line st)))
    (case  si:xr-list-so-far
	   (:toplevel
	    (values )) ;`(tr-comm ,string))) temporary fix until I figure out something better.(PHD 9/4)
	   (:after-dot
	    (values ))
	   (otherwise
	    (let* ((len (length si:xr-list-so-far))
		   (place (cdr (assoc len (gethash si:xr-list-so-far *hook-hash* ) :test #'eq))))
	      ; if there is already a comment push this one at the end of the list
	      (if place (rplacd (last place ) (list  string ))
		  (setf (gethash si:xr-list-so-far *hook-hash* )
			(acons len (list string) (gethash si:xr-list-so-far *hook-hash* ))))
	      (values ))))))

(defun read-font-change (st char)
  (declare (ignore char))
  (let ((char (read-char st)))
    (if (eq ':toplevel si:xr-list-so-far)
	;; create the font change symbol
	(values (intern (string-append #.(string #\epsilon) char)))
	(let* ((len (if (listp  si:xr-list-so-far)
			(length si:xr-list-so-far)
			1))
		(place (cdr (assoc len (gethash si:xr-list-so-far *hook-hash* )))))
	  ; if there is already a comment push this one at the end of the list
	  (if place
	      (rplacd (last place ) (list  char ))
	      (setf (gethash si:xr-list-so-far *hook-hash* )
		     (acons len (list char) (gethash si:xr-list-so-far *hook-hash* ))))
	  (values )))))

;;;PHD 6/26/86 Added support for #. and #,
;;;The form following #. or #, is translated upon reading (see tranlate-sharp-dot).
;;;The pprinter is handling sharp-dot and sharp-comma to print them back as #. and #,
;;;Note the translation handlers for sharp-dot and sharp-comma, they are there to prevent
;;;a second translation during  the translation pass.
;;;Note that we have to translate  during reading because #. can be found anywhere, like
;;;inside a quoted expression.
(defun translate-sharp-dot (st char ignore)
  (declare (ignore ignore))
  `(,(case (int-char char)
       (#\. 'sharp-dot)
       (#\, 'sharp-comma))
    ,(let ((exp (read st t nil t)))
       (translate-form exp nil)
       (transpose-package exp)
       exp)))

(deftranslation sharp-dot (form)
  (declare (ignore form))
  (nlam))

(deftranslation sharp-comma (form)
  (declare (ignore form))
  (nlam))

;; 12/08/88 DNG - Added LOCATION argument.
(defun pp-objify-sharp-dot  (object location currlevel)
  (let ((argument (si:pp-objify (cadr object) location currlevel))
	(sharp (si:make-pp-obj :type 'string :length 2 :object "#." :location location)))
      (si:make-pp-obj :type 'complex
		   :length (+ (si:pp-obj-length argument) 2)
		   :object (cons sharp
				 (if (eq (si:pp-obj-type argument) 'complex)
				     (si:pp-obj-object argument)
				     (list argument)))
		   :callish (si:pp-obj-callish argument)
		   :location location)))
(defun pp-objify-sharp-comma  (object location currlevel)
  (let ((argument (si:pp-objify (cadr object) location currlevel))
	(sharp (si:make-pp-obj :type 'string :length 2 :object "#," :location location)))
      (si:make-pp-obj :type 'complex
		   :length (+ (si:pp-obj-length argument) 2)
		   :object (cons sharp
				 (if (eq (si:pp-obj-type argument) 'complex)
				     (si:pp-obj-object argument)
				     (list argument)))
		   :callish (si:pp-obj-callish argument)
		   :location location)))


(si:defprint sharp-dot  (si:pprint-handler pp-objify-sharp-dot))
(si:defprint sharp-comma  (si:pprint-handler pp-objify-sharp-comma))
(set-dispatch-macro-character  #\# #\. 'translate-sharp-dot  tr-read)
(set-dispatch-macro-character  #\# #\, 'translate-sharp-dot  tr-read)
(set-dispatch-macro-character  #\# #\. 'translate-sharp-dot  tr-cl-read)
(set-dispatch-macro-character  #\# #\, 'translate-sharp-dot  tr-cl-read)


;(set-syntax-from-char (char-int #\/) (char-int #\\) tr-read)

(SET-MACRO-CHARACTER #\; #'read-comment nil tr-read) ; notice the bug in set-macro-character
(SET-MACRO-CHARACTER #\epsilon 'read-font-change nil tr-read) ; notice the bug in set-macro-character
(SET-MACRO-CHARACTER #\; #'read-comment nil tr-cl-read) ; notice the bug in set-macro-character
(SET-MACRO-CHARACTER #\epsilon 'read-font-change nil tr-cl-read) ; notice the bug in set-macro-character

(SET-DISPATCH-MACRO-CHARACTER  #\# #\/
			      (GET-DISPATCH-MACRO-CHARACTER
				 #\# #\\
				si::common-lisp-readtable)
			      tr-read)
(SET-DISPATCH-MACRO-CHARACTER #\#  #\\
			      (GET-DISPATCH-MACRO-CHARACTER
				 #\# #\\
				si::common-lisp-readtable)
			      tr-read)

#-elroy
(DEFUN GRIND-COMMENT (EXP LOC)
  (declare (ignore loc))
  (si::GRIND-FORM (second EXP) (LOCF (second EXP)))
  (loop for i in (nthcdr 2 exp) do
       (si::gtyo 59)
       (si::gstring i (locf i))
       (funcall si::grind-io :fresh-line)))


(defun splice-comment (form)
  (when (consp form)
    (do ((for form (if (consp for )(cdr for) nil)))
	((null for))
      (splice-comment (if (atom for) for (car for))))
;      (loop for i in form do (splice-comment i))
      (loop for i in (nreverse (GETHASH form *hook-hash*))
	    do (setf (nth (1- (min (length form) (car i))) form )
		     `(tr-comm ,(nth (1- (min (length form) (car i))) form) ,@(cdr i))))))


;; basic set of special forms handling
(deftranslation quote (form )
  (declare (special context) )
  (when (keywordp (second form))
    (min-nsubst-eq (second form) form (car context)))
  (nlam))

(deftranslation ticl:named-lambda (form)
  (dolist (l (third form)) ;; loop through the arglist to find forms to translate
	    (when (consp l) (dothis (second l) l)))
  (dothese (cdddr form) form))

(deftranslation setf  (form ) (dothese (cdr form) form));first approximation
(deftranslation setq  (form ) (dothese (nthcdr 2  form) form))

(deftranslation defun  (form )
  (let ((new-form (compiler::defun-compatibility (cdr form))))
    (if (eq (cdr new-form)(cdr  form))
	(progn
	  ;; loop through the arglist to find forms to translate
	  (dolist (l (third form))
	    (when (consp l) (dothis (second l) l)))
	  ;; do the body
	  (dothese (cdddr form) form))
	(progn
	  (change form new-form)
	  (translate form)))))

;; 1/14/89 DNG - Bind *translate-backquote* true for the body of a DEFMACRO.
(deftranslation defmacro (form)
   ;; loop through the arglist to find forms to translate
  (do ((sub-forms (THIRD form) (if (consp sub-forms ) (cdr sub-forms) nil)))
	    ((ATOM sub-forms))
    (when (and(consp (CAR sub-forms))
	      (CONSP (CdAR sub-forms)))
      (dothis (second (CAR sub-forms)) sub-forms)))
  (let ((*translate-backquote* t))
    (dothese (cdddr form) form)))

(deftranslation defmacro-displace (form)
   ;; loop through the arglist to find forms to translate
  (change (car form) 'defmacro)
  (do ((sub-forms (THIRD form) (if (consp sub-forms ) (cdr sub-forms) nil)))
	    ((atom sub-forms))
	  (when (AND (consp (CAR sub-forms))
		     (CONSP (CdAR sub-forms)))
	    (dothis (second (CAR sub-forms)) sub-forms)))
  (dothese (cdddr form) form))
(deftranslation defsubst (form) (dothese (cdddr form) form))
(deftranslation defmethod (form) (dothese (cdddr form ) form))
(deftranslation declare  (form )
  (declare (special context))
  (if (null context) ;; we are at top level so replace by proclaim
      (change form `(proclaim ',@(cdr form))))
  t) ;stop the sweep
(deftranslation the (form) (dothis (third form)form)) ; first approximation
(deftranslation lambda (form) (dothese (cddr form) form))
(deftranslation macro (form) (dothese (cdddr form) form))
(deftranslation case (form)
  (dothis (second form) form)
  (loop for i in (cddr form)
	do (dothese (cdr i) form))
  t) ;stop the sweep
(deftranslation cond (form)
  (loop for i in (cdr form) do (dothese i form))
  t)

(deftranslation do* (form)
  (loop for i in  (second form)
	do (dothese i form))
  (dothese (third form) form)
  (dothese (nthcdr 3 form)form))

(deftranslation dolist (form)
  (dothese (cdr-safe (cadr-safe form)) form)
  (dothese (nthcdr 2 form) form))

(deftranslation prog (form)
  (flet ((check-for-tags (forms)
	   ;; Will look for tags in the body of a prog.
	   (do ((sub-forms forms (if (consp sub-forms ) (cdr sub-forms) nil)))
	       ((null sub-forms))
		 (when (not (listp (car-safe sub-forms)))
		   (return t)))))
    (if (and (cadr-safe form) (symbolp (cadr-safe form))) ;; case of a named-prog
	(if (or (cadr-safe (cdr-safe form)) (check-for-tags (nthcdr 3 form)))
	    (change form `(block ,(cadr-safe form) ;tags or binding list
			    (prog ,@(nthcdr 2 form))))
	    (change form `(block nil (block ,(cadr-safe form) ,@(nthcdr 3 form) nil))))
	(if (or (cadr-safe form)(check-for-tags (nthcdr 2 form)))
	    (progn ;tags or binding list
	      (dolist (l (second form))
		(when (consp l) (dothis (second l) l)))
	      (dothese (cddr form) form))
	    (change form `(block NIL ,@(nthcdr 2 form) nil))))))

(deftranslation prog* (form)
  (dolist (l (second form))
    (when (consp l) (dothis (second l) l)))
  (dothese (cddr form) form))

(deftranslation let (form)
  (dolist (l (second form))
    (when (consp l) (dothis (second l) l)))
  (dothese (cddr form) form))

(deftranslation let* (form)
  (dolist (l (second form))
    (when (consp l) (dothis (second l) l)))
  (dothese (cddr form) form))

;; 12/13/88 DNG - Added handling for FLET, etc.
(deftranslation flet translate-flet)
(deftranslation labels translate-flet)
(deftranslation macrolet translate-flet)

(defun translate-flet (form)
  (let ((function-list (second form))
	(body (cddr form)))
    (when (consp function-list)
      (dolist (fd function-list)
	(when (consp fd)
	  (dolist (l (second fd)) ;; loop through the arglist to find forms to translate
	    (when (consp l) (dothis (second l) form)))
	  (dothese (cddr fd) form))))
    (dothese body form)))

;; 1/14/89 DNG
(deftranslation once-only (form)
  (dothese (cddr form) form))
