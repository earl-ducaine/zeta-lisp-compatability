
(in-package :transl)

(defun str (&rest rest)
  (apply #'concatenate 'string rest))

;; (proclaim '(optimize (safety 0) (speed 3)))


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

(defvar zwei::*interval*)

;; The change in this function skips over blank and comment lines
;; before starting to read the next s-expression.
(defun stream-out-next-expression ()
  "Return a stream for the next s-expression and advance point"
  (prog1 (zwei::interval-stream (zwei:skip-over-blank-lines-and-comments (zwei::point))
			  (zwei::interval-last-bp zwei::*interval*) t t)
	 (zwei::com-forward-sexp)))

(defvar si:*read-discard-font-changes*)
(defvar si:*lisp-mode*)
(defvar zwei::*window*)

;; The change-occured check avoids all the grinding lossage if there
;; are no changes. The with-undo-save means that one can answer Y and
;; still go back to the original with a single UNDO.
(hemlock-internals:defcommand ("Translate Next Sexp" com-translate-next-sexp)
    (p)
    "Translate Next Sexp in Buffer"
    "Translate Next Sexp in Buffer"
  (let((*context* nil)
       (*trace* nil)
       (change-occured nil)
       (*translate-backquote* nil))
    ;;(declare (special context trace change-occured))
    (clrhash *hook-hash*)
    ;; phd 3/15/86 #(1 2 3) will be printed correctly
    (let* ((*print-array* t)
	   (stream (stream-out-next-expression))
	   (form (let (;; We don't (although we could have) a zetalisp
		       ;; readtable.
		       ;;
		       ;; (*readtable* (if (zetalisp-on-p )
		       ;; 			tr-read
		       ;; 			tr-cl-read))
		       (si:*read-discard-font-changes* nil))
		   ;; Final item is discard open parens (t), which
		   ;; isn't an option in the ANSI CL reader.
		   ;; (read  stream nil '*EOF* nil nil t)
		   (read  stream nil '*EOF*))))
      (if (or (null form)
	      (EQ FORM '*EOF*))
	  (zwei::BARF (str "there is no form to translate ; position "
			   "the cursor at the beginning of a form"))
	  (progn
	    (translate-form form nil)
	    (transpose-package form)
	    (cond ((null change-occured)
		   (format *query-io* "~&No changes necessary."))
		  (t
		   (splice-comment form)
		   (zwei:with-undo-save
		       ("Translation" (zwei:forward-sexp (zwei:point) -1)
				      (zwei:point) t)
		     (let ((*readtable* #.*readtable*)
			   (si:*lisp-mode* :common-lisp)
			   (si:*reader-symbol-substitutions*
			    si:*COMMON-LISP-SYMBOL-SUBSTITUTIONS* ))
		       (pprint form
			       (zwei::INTERVAL-STREAM-INTO-BP (ZWei::point) t)))
		     (zwei::must-redisplay zwei::*window* zwei::dis-text)
		     (zwei::redisplay zwei::*window* )
		     (if (y-or-n-p "Is this ok?")
			 (progn
			   (hemlock::backward-kill-form-command)
			   (hemlock::kill-line-command)
			   (hemlock::forward-form-command))
			 (hemlock::forward-kill-form-command))))))))
    zwei::dis-text))

(hemlock-internals:defcommand
    ("Translate Next Sexp Including Backquotes"
     com-translate-next-sexp-including-backquotes) (p)
    "Translate Next Sexp Including Backquotes"
    "Translate Next Sexp Including Backquotes"
  (declare (ignore p))
  (let((*context* nil)
       (*trace* nil)
       (change-occured nil)
       (*translate-backquote* t))
    (clrhash *hook-hash*)
    ;; phd 3/15/86 #(1 2 3) will be printed correctly
    (let* ((*print-array* t)
	   (stream (stream-out-next-expression))
	   (form (let ((*readtable* (if (zetalisp-on-p)
					tr-read
					tr-cl-read))
		       (si:*read-discard-font-changes* nil))
		   ;; The final argument in zetalisp indicates that
		   ;; read should ignore unmatched trailing right
		   ;; parens. Doesn't exist in ansi CL.
		   ;; (read  stream nil '*EOF* nil nil t)
		   (read  stream nil '*eof*))))
      (if (or (null form)
	      (eq form '*eof*))
	  (zwei::BARF (str "there is no form to translate ; "
			   "position the cursor at the beginning of a form"))
	  (progn
	    (translate-form form nil)
	    (transpose-package form)
	    (cond ((null change-occured)
		   (format *query-io* "~&No changes necessary."))
		  (t
		   (splice-comment form)
		   (zwei:with-undo-save
		       ("Translation" (zwei:forward-sexp (zwei:point) -1)
				      (zwei:point) t)
		     (let ((*readtable* #.*readtable*)
			   (si:*lisp-mode* :common-lisp)
			   (si:*READER-SYMBOL-SUBSTITUTIONS*
			    si:*COMMON-LISP-SYMBOL-SUBSTITUTIONS*))
		       (pprint form (zwei::INTERVAL-STREAM-INTO-BP
				     (zwei::point)
				     t)))
		     (zwei::must-redisplay zwei::*window* zwei::dis-text)
		     (zwei::redisplay zwei::*window* )
		     (if (y-or-n-p "Is this ok?")
			 (progn
			   (hemlock::backward-kill-form-command)
			   (hemlock::kill-line-command)
			   (hemlock::forward-form-command))
			 (hemlock::forward-kill-form-command))))))))
    zwei::dis-text))

;; (zwei:defcom com-install-translator-key
;; 	     "Install the Hyper-T key in the current comtab" ()
;;   (zwei::set-comtab zwei::*comtab* `(|hyper-T| com-translate-next-sexp))
;;   (zwei::set-comtab zwei::*comtab* `(|hyper-super-T| com-translate-next-sexp-including-backquotes))
;;   zwei::dis-none)

;; Bind translation to Hyper-T/Hyper-Super-T is bound to the
;; translation command. #k is the defined in the hemlock readtable
(hemlock-internals:defcommand "Install Translate on Hyper-T" (p)
    "Install Translate on Hyper-T"
    "Install Translate on Hyper-T"
    (declare (ignore p))
    (hemlock-interface:bind-key "Translate Next Sexp" #k"hyper-T")
    (hemlock-interface:bind-key "Translate Next Sexp Including Backquotes"
				#k"hyper-super-T"))

(si:defprint transl:tr-comm (si:pprint-handler si:pp-objify-comment))

(defvar si:*reader-symbol-substitutions*)
(defvar *print-structure*)
(defvar *trace*)
(defvar si::*read-discard-font-changes*)

;; 12/08/88 DNG - Original version.
;;  1/05/89 DNG - Avoid possibility of doing the defsystem file twice.
;;  1/14/89 DNG - Use keyword arguments.
(defun translate-system (system-name output-directory
			 &key
			   (trace nil)
			   base
			   (case *print-case*)
			   (radix *print-radix*))
  "Translates all of the files in a system from Zetalisp to Common Lisp.
   The first argument is the name of the system, and the second is the
   pathname of the directory where the translated files are to be
   written. trace set to t will cause all changes to be printed out on
   *standard-output* base indicates the print base of the translator,
   nil means use the file's base. case is the value for *print-case*
   -- either :upcase or :downcase. radix is the value for
   *print-radix* -- true to include explicit radix on numbers."
  (let* ((files (asdf-helper:system-files system-name
				  ;; todo -- Currently system returns
				  ;; all components. We should control
				  ;; by filter at some time
				  '(:recompile :do-not-do-components)
				  '(:compile :readfile)))
	 (system (asdf:find-system system-name t t))
	 (ofiles '()))
    (unless (null system)
      (pushnew (asdf/system::system-source-file system)
	       files :test #'equal))
    (dolist (file files)
      (with-simple-restart
	  (nil
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
	(when (rassoc (car sexp)
		     si:*zetalisp-symbol-substitutions*
		     :test #'eq)
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
		 (progn
		   (setf new-car (min-nsubst-eq new old (car s)))
		   (when (and (neq new-car (car s))
			      (check-for-fef-area s))
		     (setf (car s) new-car)))))
	   s-exp)))

(defvar *change-occured*)
(defvar *context*)

(defun trace-change (form exp)
  (setf *change-occured* t)
  (when (and (boundp 'trace)
	     (symbol-value 'trace)
	     *context*)
    (let ((*print-length* 3)
	  (*print-level* 2)
	  (*readtable* #.*readtable*))
      (format *standard-output* "~%In ~S~%	 ~S => ~S"
	      (car (last *context*)) form exp))))

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
	  (translate-form (if (consp sub-forms)
			      (car sub-forms)
			      sub-forms)
			  context)))))

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

(defvar si:*common-lisp-symbol-substitutions*)

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


;; Todo not clear how to interpret this maclisp style of defun.
;;
;; (defmacro deftranslation (name parm-list &body body)
;;   (if (and (not (listp parm-list))
;; 	   (null body))
;;       `(defprop ,name ,parm-list transform)
;;     `(defun (:property ,name transform)
;; 	    ,parm-list ,@body)))
;;
;; So, we'll just convert it to defun and see what doesn't work.
(defmacro deftranslation (name parm-list &body body)
  (if (and (not (listp parm-list))
	   (null body))
      `(defprop ,name ,parm-list transform)
      `(eval-when (:load-toplevel :compile-toplevel :execute)
	 (setf (get ',name 'transform)
	       (lambda  ,parm-list ,@body)))))

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

;; (p-defgeneric-options
;;  add-method
;;  ((generic-function
;;    (if (typep generic-function 'generic-function)
;;        generic-function
;;        (get-generic-function-object generic-function)))
;;   method)
;;  (:documentation "Add a method to a generic function.
;; The generic function is destructively modified and returned as the result."))


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
	(values (intern (str #.(string '|epsilon|) char)))
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

;;; PHD 6/26/86 Added support for #. and #, The form following #. or
;;; #, is translated upon reading (see tranlate-sharp-dot).  The
;;; pprinter is handling sharp-dot and sharp-comma to print them back
;;; as #. and #, Note the translation handlers for sharp-dot and
;;; sharp-comma, they are there to prevent a second translation during
;;; the translation pass. Note, we have to translate during
;;; reading because #. can be found anywhere, like inside a quoted
;;; expression.
(defun translate-sharp-dot (st char ignore)
  (declare (ignore ignore))
  `(,(case (code-char char)
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

;; todo -- stub out for the moment, until we have an example we can
;; study in detail.
;; (defun pp-objify-sharp-dot  (object location currlevel)
;;   (let ((argument (si:pp-objify (cadr object) location currlevel))
;; 	(sharp (si:make-pp-obj :type 'string :length 2 :object "#." :location location)))
;;       (si:make-pp-obj :type 'complex
;; 		   :length (+ (si:pp-obj-length argument) 2)
;; 		   :object (cons sharp
;; 				 (if (eq (si:pp-obj-type argument) 'complex)
;; 				     (si:pp-obj-object argument)
;; 				     (list argument)))
;; 		   :callish (si:pp-obj-callish argument)
;; 		   :location location)))
(defun pp-objify-sharp-dot  (object location currlevel)
  (declare (ignore object location currlevel)))


;; todo -- stub out for the moment, until we have an example we can
;; study in detail.
;; (defun pp-objify-sharp-comma  (object location currlevel)
;;   (let ((argument (si:pp-objify (cadr object) location currlevel))
;; 	(sharp (si:make-pp-obj :type 'string :length 2 :object "#," :location location)))
;;       (si:make-pp-obj :type 'complex
;; 		   :length (+ (si:pp-obj-length argument) 2)
;; 		   :object (cons sharp
;; 				 (if (eq (si:pp-obj-type argument) 'complex)
;; 				     (si:pp-obj-object argument)
;; 				     (list argument)))
;; 		   :callish (si:pp-obj-callish argument)
;; 		   :location location)))
(defun pp-objify-sharp-comma  (object location currlevel)
  (declare (ignore object location currlevel)))

;; todo -- stub out pprinting stuff until we have a concrete example
;; of how it's used
;; (si:defprint sharp-dot  (si:pprint-handler pp-objify-sharp-dot))
;; (si:defprint sharp-comma  (si:pprint-handler pp-objify-sharp-comma))

(set-dispatch-macro-character  #\# #\. 'translate-sharp-dot  tr-read)
(set-dispatch-macro-character  #\# #\, 'translate-sharp-dot  tr-read)
(set-dispatch-macro-character  #\# #\. 'translate-sharp-dot  tr-cl-read)
(set-dispatch-macro-character  #\# #\, 'translate-sharp-dot  tr-cl-read)


;(set-syntax-from-char (char-int #\/) (char-int #\\) tr-read)

;; notice the bug in set-macro-character
(set-macro-character #\; #'read-comment nil tr-read)

;; ed: doubtful this is thiss the case: notice the bug in set-macro-character
;; todo (ed) -- another example of case that we don't know how to handle
;; and that we'll need a contcrete example of to properly implement.
;; (set-macro-character '|epsilon| 'read-font-change nil tr-read)

;; notice the bug in set-macro-character
(set-macro-character #\; #'read-comment nil tr-cl-read)

;; ed: doubtful this is thiss the case: notice the bug in set-macro-character
;; todo (ed) -- another example of case that we don't know how to handle
;; and that we'll need a contcrete example of to properly implement.
;; (set-macro-character '|epsilon| 'read-font-change nil tr-cl-read)

(set-dispatch-macro-character  #\# #\/
			      (get-dispatch-macro-character
				 #\# #\\
				si::*common-lisp-readtable*)
			      tr-read)

(SET-DISPATCH-MACRO-CHARACTER #\#  #\\
			      (GET-DISPATCH-MACRO-CHARACTER
				 #\# #\\
				si::*common-lisp-readtable*)
			      tr-read)

#-elroy
(defun grind-comment (exp loc)
  (declare (ignore loc))
  (si::grind-form (second exp) (locf (second exp)))
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


;;; basic set of special forms handling

;;; todo, these try to redefine quote, setf, setq which isn't
;;; permitted. We'll have to use a method compatible with ANSI CL and
;;; SBCL instead, function wrapper? Shadowing?

(deftranslation ticl:quote (form)
  (declare (special context) )
  (when (keywordp (second form))
    (min-nsubst-eq (second form) form (car context)))
  (nlam))

(deftranslation ticl:named-lambda (form)
  (dolist (l (third form)) ;; loop through the arglist to find forms to translate
	    (when (consp l) (dothis (second l) l)))
  (dothese (cdddr form) form))

(deftranslation ticl:setf  (form ) (dothese (cdr form) form));first approximation
(deftranslation ticl:setq  (form ) (dothese (nthcdr 2  form) form))

(deftranslation ticl:defun  (form )
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
(deftranslation ticl:defmacro (form)
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
(deftranslation ticl:defmethod (form) (dothese (cdddr form ) form))
(deftranslation ticl:declare  (form )
  (declare (special context))
  (if (null context) ;; we are at top level so replace by proclaim
      (change form `(proclaim ',@(cdr form))))
  t) ;stop the sweep
(deftranslation ticl:the (form) (dothis (third form)form)) ; first approximation
(deftranslation ticl:lambda (form) (dothese (cddr form) form))
(deftranslation ticl:macro (form) (dothese (cdddr form) form))
(deftranslation ticl:case (form)
  (dothis (second form) form)
  (loop for i in (cddr form)
	do (dothese (cdr i) form))
  t) ;stop the sweep
(deftranslation ticl:cond (form)
  (loop for i in (cdr form) do (dothese i form))
  t)

(deftranslation ticl:do* (form)
  (loop for i in  (second form)
	do (dothese i form))
  (dothese (third form) form)
  (dothese (nthcdr 3 form)form))

(deftranslation ticl:dolist (form)
  (dothese (cdr-safe (cadr-safe form)) form)
  (dothese (nthcdr 2 form) form))

(deftranslation ticl:prog (form)
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

(deftranslation ticl:prog* (form)
  (dolist (l (second form))
    (when (consp l) (dothis (second l) l)))
  (dothese (cddr form) form))

(deftranslation ticl:let (form)
  (dolist (l (second form))
    (when (consp l) (dothis (second l) l)))
  (dothese (cddr form) form))

(deftranslation ticl:let* (form)
  (dolist (l (second form))
    (when (consp l) (dothis (second l) l)))
  (dothese (cddr form) form))

;; 12/13/88 DNG - Added handling for FLET, etc.
(deftranslation ticl:flet translate-flet)
(deftranslation ticl:labels translate-flet)
(deftranslation ticl:macrolet translate-flet)

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
