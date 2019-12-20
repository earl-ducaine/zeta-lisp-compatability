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

;;;Edited by RICE                  24 Jan 90  13:12
;;;Edited by RICE                  20 Feb 90  17:38
(defun get-value (x)
  (if (consp x)
      (if (member :value x)
	  (second (member :value x))
	  (if (consp (rest x))
	      (second x)
	      (first x)
	  )
      )
      x
  )
)

(defun accept->help-string (accept-spec stream)
  (if (and (consp (rest accept-spec)) (getf (rest accept-spec) :documentation))
      (format stream "~%~A~20T~A" (first accept-spec)
	      (getf (rest accept-spec) :documentation)
      )
      (format stream "~%~A" (first accept-spec))
  )
)

(defmethod type-arg-for-presentation ((object T))
  (type-of object)
)

(defmethod type-arg-for-presentation ((object symbol))
  (if (get object 'presentation)
      object
      (clos:call-next-method)
  )
)

(defmethod Type-Arg-For-Presentation ((object clos:standard-object))
  object
)

(defmethod Type-Arg-For-Presentation ((object clos:structure-object))
  object
)

(defmethod Type-Arg-For-Presentation ((object vanilla-flavor))
  object
)

(defmethod specializer-arg-for-presentation ((object T))
  (if (or (not (type-specifier-p object))
	  (and (type-specifier-p object)
	       (not (clos:find-class object nil))
	  )
	  (typep (clos:find-class object nil) 'clos:built-in-class)
      )
      `(eql ',object)
      object
  )
)

(clos:defgeneric Mouse-Click-Processor
		 (object presentationc window item button x y)
)

;;;Edited by RICE                  2 Jan 90  17:59
(defun Data-And-Pr-Arglist-Body
       (data-arglist data-arglist-name pr-arglist pr-arglist-name body)
  `(destructuring-bind (,@data-arglist) ,data-arglist-name
     (declare (unspecial ,@(Just-Arg-Names data-arglist)))
     (destructuring-bind (,@pr-arglist) ,pr-arglist-name
       (declare (unspecial ,@(Just-Arg-Names pr-arglist)))
       ,@(Ignore-meta-args)
       ,@body
     )
   )
)

(defun help-function-for (presentation-type body)
 `((with-help-function
     (#'(lambda (stream)
	  (format stream "~&You are being asked to enter ~A.  "
		  (or description (type-pretty-name ',presentation-type))
	  )
	)
     )
     ,@body
  ))
)

;;;Edited by RICE                  18 Dec 89  15:00
(defun Make-Parser-Method
       (presentation-type parser type-name data-arglist pr-arglist)
  (let ((args (if parser `(options ,@(first parser)) '(options stream)))
	(body (if parser (rest parser) '((read stream))))
       )
     `(defun (:Property ,type-name sys:prompt-and-read-function) ,args
	(block ,presentation-type
	  (let ((data-arglist (second options))
		(pr-arglist (rest (rest options)))
	       )
	       (let ((result
		       (catch 'tv:return-from-rubout-handler
			 ,(Data-And-Pr-Arglist-Body
			    data-arglist 'data-arglist pr-arglist 'pr-arglist
			    (Help-Function-For presentation-type body)
			  )
		       )
		     )
		    )
		    (if (and (consp result)
			     (equal (first result) :mouse-click-input)
			)
			;; remove the mouse char.
			(progn (tv:io-buffer-get (send ,(first args) :Io-Buffer))
			       (second result)
			)
			result
		    )
	       )
	  )
	)
      )
  )
)


(defmethod least-general-read-function-for-1 ((type t) data-arglist pr-arglist)
  (ignore data-arglist pr-arglist)
  'read
)

(defun least-general-read-function-for (presentation)
  (let ((new-type (non-short-form-presentation presentation)))
       (destructuring-bind ((type &rest data-arglist) pr-arglist) new-type
	 (least-general-read-function-for-1 type data-arglist pr-arglist)
       )
  )
)

(defun make-parser-method-from-expander
       (presentation-type expander type-name)
 `(defun (:Property ,type-name sys:prompt-and-read-function) (options stream)
    (ignore options)
    (let ((expansion (Non-Short-Form-Presentation ,expander)))
	 (let ((data-arglist (rest (first expansion)))
	       (pr-arglist (rest expansion))
	       (type (first (first expansion)))
	       (arg-data-arglist (second options))
	       (arg-pr-arglist (rest (rest options)))
	      )
	      (ignore arg-data-arglist arg-pr-arglist)
	      (Let ((result
		      (catch 'tv:return-from-rubout-handler
			,(Data-And-Pr-Arglist-Body
			   nil nil `(&key ,@*meta-presentation-arguments*)
			   'pr-arglist
			   (Help-Function-For
			     presentation-type
			     `((Accept-Named-Presentation
				 type stream data-arglist pr-arglist
			       ))
			   )
			 )
		      )
		    )
		   )
		   (if (and (consp result)
			    (equal (first result) :mouse-click-input)
		       )
		       ;; remove the mouse char.
		       (progn (tv:io-buffer-get (send stream :Io-Buffer))
			      (second result)
		       )
		       result
		   )
	      )
	 )
    )
  )
)

(clos:defgeneric Print-Named-Presentation
		 (type data-arglist object stream &rest args)
)

;;;Edited by RICE                  2 Jan 90  17:59
(defun transform-ignores (arglist)
  (let ((transformed (loop for arg in arglist
			   for index from 0
			   collect (if (equal 'ignore arg) (intern (format nil "IGNORE-~D" index)) arg)
		     )
	)
       )
       transformed
  )
)

;;;Edited by RICE                  2 Jan 90  17:59
(defun Make-Printer-Method
       (printer specializer type-name pr-arglist)
  (destructuring-bind (obj stream) (transform-ignores (first printer))
   `(defmethod print-named-presentation
	((type ,specializer) data-arglist ,obj ,stream &rest args)
      (ignore data-arglist)
      (flet ((_body_ (,obj ,stream ,@(transform-ignores pr-arglist))
	      (declare (unspecial ,obj ,stream ,@(Just-Arg-Names (transform-ignores pr-arglist))))
	      ,@(append (Ignore-meta-args) (list obj stream))
	      ,@(rest printer)
	     )
	    )
	    (if (send ,stream :Operation-Handled-P :Print)
		(send ,stream :Print ,obj
		      *prindepth* *print-escape*
		      :Print-Function
		      #'(lambda (object depth str which-ops)
			  (let ((*prindepth* depth))
			       (ignore which-ops)
			       (apply #'_body_ object str args)
			  )
			)
			:Item-Type ',type-name
		)
		(apply #'_body_ ,obj ,stream args)
	    )
      )
    )
  )
)

(defun make-printer-methods (printer specializer type-name pr-arglist)
  (if (consp specializer) ;;; eql
      (list (make-printer-method printer specializer type-name pr-arglist))
      (list (make-printer-method printer specializer type-name pr-arglist)
	    (make-printer-method printer `(eql ',specializer)
				 type-name pr-arglist
	    )
      )
  )
)

(defun add-globally-to-item-type-alist (type-name)
  `((locally (declare (special w:*live-listener-item-type-alist*))
      (if (boundp 'w:*live-listener-item-type-alist*)
	  (let ((entry (list ',type-name :Printed-Object
			     '(w:live-listener-wholine-function) nil
			 )
		)
	       )
	       (if (not (member entry w:*live-listener-item-type-alist*
				:Test #'equalp
			)
		   )
		   (nconc w:*live-listener-item-type-alist* (list entry))
	       )
	  )
	  nil
      )
    )
   )
)

(clos:defgeneric display-possibilities (type stream data-arglist pr-arglist))

(defmethod Display-Possibilities (type stream data-arglist pr-arglist)
  (ignore type stream data-arglist pr-arglist)
  nil
)

(clos:defgeneric Accept-Named-Presentation
		 (type stream data-arglist pr-arglist)
)

;;;Edited by RICE                  2 Jan 90  17:59
(defun ignore-meta-args ()
 `((ignore ,@(mapcar #'first *Meta-Presentation-Arguments*)))
)

(Define-meta-presentation-argument Description nil)
(Define-meta-presentation-argument Default nil)
(Define-meta-presentation-argument provide-default nil)

;;;Edited by BuildCyc              11 Dec 89  12:13
(defun ticl:present (object
		     &optional
		       (presentation-type (Type-Arg-For-Presentation object))
		     &Key (stream *standard-output*)
		          (acceptably nil)
			  (sensitive t)
			  (form nil)
			  (location nil)
			  (single-box nil)
			  (allow-sensitive-inferiors t)
		     )
  (ignore acceptably sensitive form location single-box
	  allow-sensitive-inferiors
  )
  (let ((presentation-type
	  (if (consp presentation-type)
	      presentation-type
	      (list
		(list (or presentation-type (Type-Arg-For-Presentation object)))
	      )
	  )
	)
       )
       (apply 'Print-Named-Presentation (first (first presentation-type))
              (rest (first presentation-type))
	      object stream (rest presentation-type)
       )
  )
)
(if (find-package 'mm) (shadow 'ticl:present 'mm))
(export 'ticl:present 'ticl)

(defvar *recursive-call* nil)

;;;Edited by LOW                   22 Jan 90  17:51
;;;Edited by LOW                   22 Jan 90  18:00
;;;Edited by RICE                  24 Jan 90  13:12
(defun ticl:accept (presentation-type
		   &key (stream *query-io*)
		        (prompt :enter-type)
			(prompt-mode :normal)
			(activation-chars nil)
			(additional-activation-chars nil)
			(blip-chars nil)
			(additional-blip-chars nil)
			(inherit-context t)
			(default nil)
			(provide-default nil)
			(default-type presentation-type)
			(display-default 'prompt)
			(present-default nil)
			(prompts-in-line *recursive-call*)
			(initially-display-possibilities nil)
			(input-sensitizer nil)
			(handler-type nil) ;;; Fix this
			(query-identifier nil)
			(separate-inferior-queries nil)
		  )
  (ignore activation-chars additional-activation-chars
	  blip-chars additional-blip-chars inherit-context
	  initially-display-possibilities input-sensitizer handler-type
	  query-identifier separate-inferior-queries
  )
  (let ((presentation-type (Non-Short-Form-Presentation presentation-type))
	(*default-presentation-type* default-type)
        (*Recursive-Call* t)
       )
       (Declare (special *default-presentation-type*
			 *Recursive-Call*
		)
       )
       (If (and prompt prompts-in-line)
	   (format *query-io* "(")
	   nil
       )
       (etypecase prompt
	 (string (princ prompt stream))
	 (function (funcall prompt stream :Accept))
	 (symbol
	  (ecase prompt
	    (:Enter-Type
	     (format stream "Enter ")
	     (describe-presentation (first (first presentation-type))
				    (rest (first presentation-type)) stream
	     )
	    )
	    (nil nil)
	  )
	 )
       )
       (if initially-display-possibilities
	   (progn (format stream " ")
		  (display-possibilities
		    (first (first presentation-type)) stream
		    (rest (first presentation-type)) (rest presentation-type)
		  )
	   )
	   nil
       )
       (let ((display-p
	       (ecase display-default
		 (prompt (or default provide-default))
		 ('t t)
		 (nil nil)
	       )
	     )
	    )
	    (if display-p
		(progn (princ " [" stream)
		       (if present-default
			   (Present default default-type :stream stream)
			   (format stream "~S" default)
		       )
		       (princ "]" stream)
		)
		nil
	    )
       )
       (if prompt
	   (ecase prompt-mode
	     (:Normal
	      (if prompts-in-line (format stream ") ") (format stream ": "))
	     )
	     (:Raw (if prompts-in-line (format stream ") ") nil))
	   )
	   nil
       )
       (let ((*extra-pr-args*
	       `(:Default ,default
		 :provide-default ,provide-default
		)
	     )
	    )
	    (declare (special *extra-pr-args*))
	    (Accept-Named-Presentation (first (first presentation-type)) stream
	      (rest (first presentation-type))
	      (append (rest presentation-type) *extra-pr-args*)
	    )
       )
  )
)

(eval-when (compile load eval)
  (shadow 'ticl:accept 'chaos)
  (export 'ticl:accept 'ticl)
)

(defun ticl:prompt-and-accept
       (presentation-type &optional format-string &rest format-args)
  (ticl:accept presentation-type
	       :Prompt #'(lambda (stream ignore)
			   (apply #'format stream format-string format-args)
			 )
	       :Prompt-Mode :normal
  )
)


(export 'ticl:prompt-and-accept 'ticl)

;-------------------------------------------------------------------------------

;;;Edited by LOW                   22 Jan 90  17:44
(defun non-short-form-presentation (type)
  (if (consp type)
      (if (consp (first type))
	  type
	  (list type)
      )
      (list (list type))
  )
)

(defmethod presented-item-typep
	   ((object t)
	    (presentation-type (eql 'not))
	    (data-arglist list)
	    (window t)
	   )
  (if data-arglist
      (let ((new-type (non-short-form-presentation (first data-arglist))))
	   (destructuring-bind ((type &rest d-arglist) pr-arglist) new-type
	     (ignore pr-arglist)
	     (not (presented-item-typep object type d-arglist window))
	   )
      )
      t
  )
)

(defmethod print-named-presentation
	   ((type (eql 'not)) data-arglist object stream &rest args)
  (apply 'Print-Named-Presentation t data-arglist object stream args)
)

(defmethod presented-item-typep
	   ((object t)
	    (presentation-type (eql 'and))
	    (data-arglist list)
	    (window t)
	   )
  (if data-arglist
      (and (let ((new-type (non-short-form-presentation (first data-arglist))))
		(destructuring-bind ((type &rest d-arglist) pr-arglist) new-type
		  (ignore pr-arglist)
		  (presented-item-typep object type d-arglist window)
		)
	   )
	   (Presented-Item-Typep
	     object presentation-type (rest data-arglist) window
	   )
      )
      t
  )
)

(defmethod print-named-presentation
	   ((type (eql 'and)) data-arglist object stream &rest args)
  (if data-arglist
      (or (catch-error
	    (let ((new-type (non-short-form-presentation (first data-arglist))))
		 (destructuring-bind
		   ((type &rest d-arglist) pr-arglist) new-type
		   (ignore pr-arglist)
		   (apply 'Print-Named-Presentation
			  type d-arglist object stream pr-arglist
		   )
		 )
		 t
	    )
	  )
	  (Print-Named-Presentation type (rest data-arglist) object stream args)
      )
      nil
  )
)

(defmethod accept-named-presentation
	   ((type (eql 'and)) stream data-arglist pr-arglist)
  (let ((*Satisfies-Read*
	  (Least-General-Read-Function-For
	    `((,type ,@data-arglist) ,@pr-arglist)
	  )
	)
       )
       (declare (special *Satisfies-Read*))
       (loop for result = (funcall *Satisfies-Read* stream)
	     until (presented-item-typep
		     result type data-arglist stream
		   )
	     do
	      (Presentation-Parse-Error "~S is not ~A" (type-pretty-name type))
	     finally (return result)
       )
  )
)


(defmethod presented-item-typep
	   ((object t)
	    (presentation-type (eql 'or))
	    (data-arglist list)
	    (window t)
	   )
  (if data-arglist
      (or (let ((new-type (non-short-form-presentation (first data-arglist))))
	       (destructuring-bind ((type &rest d-arglist) pr-arglist) new-type
		 (ignore pr-arglist)
		 (presented-item-typep object type d-arglist window)
	       )
	  )
	  (Presented-Item-Typep
	    object presentation-type (rest data-arglist) window
	  )
      )
      nil
  )
)

(defmethod accept-named-presentation
	   ((type (eql 'or)) stream data-arglist pr-arglist)
  (let ((*Satisfies-Read*
	  (Least-General-Read-Function-For
	    `((,type ,@data-arglist) ,@pr-arglist)
	  )
	)
       )
       (declare (special *Satisfies-Read*))
       (loop for result = (funcall *Satisfies-Read* stream)
	     until (presented-item-typep
		     result type data-arglist stream
		   )
	     do
	      (Presentation-Parse-Error "~S is not ~A" (type-pretty-name type))
	     finally (return result)
       )
  )
)

(defmethod print-named-presentation
	   ((type (eql 'or)) data-arglist object stream &rest args)
  (if data-arglist
      (or (catch-error
	    (let ((new-type (non-short-form-presentation (first data-arglist))))
		 (destructuring-bind
		    ((type &rest d-arglist) pr-arglist) new-type
		   (ignore pr-arglist)
		   (apply 'Print-Named-Presentation
			  type d-arglist object stream pr-arglist
		   )
		 )
		 t
	    )
	  )
	  (Print-Named-Presentation type (rest data-arglist) object stream args)
      )
      nil
  )
)

(defmethod presented-item-typep
	   ((object t)
	    (presentation-type (eql 'satisfies))
	    (data-arglist list)
	    (window t)
	   )
  (and (functionp (first data-arglist))
       (funcall (first data-arglist) object)
  )
)

(defun presentation-parse-error (format-string &rest format-args)
  (signal (apply #'make-condition 'si:parse-error format-string format-args))
)

(defvar *satisfies-read* 'read)

(defun (:Property satisfies sys:prompt-and-read-function) (options stream)
  (let ((data-arglist (second options))
	(pr-arglist (rest (rest options)))
       )
       (let ((body-function
	       #'(lambda ()
		   (loop for result =
			 (let ((res
				 (catch 'tv:return-from-rubout-handler
				   (funcall *satisfies-read* stream)
				 )
			       )
			      )
			      (if (and (consp res)
				       (equal (first res) :mouse-click-input)
				  )
				  ;; remove the mouse char.
				  (progn (tv:io-buffer-get
					   (send stream :Io-Buffer)
					 )
					 (second res)
				  )
				  res
			      )
			 )
			 until (funcall (first data-arglist) result)
			 do (Presentation-Parse-Error
			      "~S does not satisfy ~S" result
			      (first data-arglist)
			    )
			 finally (return result)
		   )
		 )
	     )
	    )
	    (let ((description
		    (getf pr-arglist :Description
		     (format nil "something satisfying ~S" (first data-arglist))
		    )
		  )
		 )
		 (with-help-function
		   (#'(lambda (stream)
			(format stream "~&You are being asked to enter ~A"
			       description
			)
		      )
		   )
		   (funcall body-function)
		 )
	    )
       )
  )
)

(defmethod accept-named-presentation
	((type (eql 'satisfies)) stream data-arglist pr-arglist)
  (let ((*query-io* stream))
       (prompt-and-read
	 (cons 'satisfies (cons data-arglist pr-arglist)) ""
       )
  )
)

(record-source-file-name 'satisfies 'presentation)
(setf (get 'satisfies 'presentation) T)


;-------------------------------------------------------------------------------

;;;Edited by RICE                  18 Dec 89  15:00
;;;Edited by RICE                  8 Jan 90  12:20
;;;Edited by LOW                   22 Jan 90  17:22
(defmacro ticl:define-presentation-type
      (type-name ((&rest data-arglist) &rest pr-arglist)
       &key (parser   nil)
            (printer  nil)
	    (description (or (type-pretty-name type-name) "anything"))
	    (describer nil)
	    (expander nil)
	    (typep    nil)
	    (no-deftype nil)
	    (history nil)
	    (abbreviation-for nil)
      )
  (ignore data-arglist no-deftype history abbreviation-for)
  (let ((specializer (Specializer-Arg-For-Presentation type-name))
	(pr-arglist
	  (append pr-arglist
		  (if (member '&key pr-arglist)
		      nil
		      '(&key)
		  )
		  *Meta-Presentation-Arguments*
	  )
	)
       )
      `(Progn (defmethod presented-item-typep
		((object ,(if (consp specializer) t specializer))
		 (presentation-type ,(if (consp specializer) specializer t))
		 (data-arglist list)
		 (stream t)
		)
		,@(if typep
		     `((destructuring-bind (,@data-arglist)
					   data-arglist
			 (declare (unspecial ,@(Just-Arg-Names data-arglist)))
			 (funcall ,typep)
		       )
		      )
		      '(t)
		  );;; item already matches through method discrimination.
	       )
	       (defmethod mouse-click-processor
		((object ,(if (consp specializer) t specializer))
		 (presentation-type ,(if (consp specializer) specializer t))
		 (window t) item button x y
		)
		(send window :process-print-blip item button x y)
		t ;;; item already matches through method discrimination.
	       )
	      ,@(add-globally-to-item-type-alist type-name)
	      ,(if parser
		   (Make-Parser-Method
		     type-name parser type-name data-arglist pr-arglist
		   )
		   (if expander
		       (make-parser-method-from-expander
			 type-name expander type-name
		       )
		       nil
		   )
	       )
	      ,@(if describer
		   `((defmethod describe-presentation
				((object ,specializer)
				 data-arglist ,@(first describer) &key data-type-args
				 presentation-args type plural-count
				)
		       (ignore data-type-args presentation-args type plural-count)
		       (destructuring-bind (,@data-arglist) data-arglist
			 (ignore ,@(just-arg-names data-arglist))
			 ,@(rest describer)
		       )
		     )
		    )
		   `((defmethod describe-presentation
				((object ,specializer)
				 data-arglist stream &key data-type-args
				 presentation-args type plural-count
				)
		       (ignore data-type-args presentation-args type plural-count data-arglist)
		       (princ ,description stream)
		     )
		    )
	        )
	       (defmethod accept-named-presentation
		       ((type (eql ',type-name)) stream data-arglist pr-arglist)
		 (let ((*query-io* stream))
		      (loop for result = 
		        (prompt-and-read
			  (cons ',type-name (cons data-arglist pr-arglist)) ""
			)
			until (presented-item-typep
				result ',type-name data-arglist stream
			      )
			do (Presentation-Parse-Error "~S is not ~A"
                             (type-pretty-name type)
			   )
			finally (return result)
		      )
		 )
	       )
	       (defprop ,type-name presented-item-typep
			w:prompt-and-read-mousable-type
	       )
	       (defprop ,type-name Mouse-Click-Processor :Mouse-Click-Function)
	      ,@(if printer
		    (Make-Printer-Methods
		      printer specializer type-name pr-arglist
		    )
		    nil
	        )
	       (record-source-file-name ',type-name 'presentation)
	       (setf (get ',type-name 'presentation) T)
	      ',type-name
       )
  )
)

(export 'ticl:define-presentation-type 'ticl)
