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

;-------------------------------------------------------------------------------

(defun activating-prompt-and-read (default-p default &rest args)
  (if default-p
      (let ((old-read #'internal-read-char))
	   (letf ((#'internal-read-char
		   #'(lambda (stream  &optional (errorp t) eofval ignore)
		       (let ((result
			       (funcall old-read stream errorp eofval nil)
			     )
			    )
			    (if (eofp result)
				(throw 'activation '**activation**)
				result
			    )
		       )
		     )
		  )
		 )
		 (let ((result (catch 'activation
				 (apply 'prompt-and-read args)
			       )
		       )
		      )
		      (if (equal '**Activation** result)
			  (values default T)
			  (values result nil)
		      )
		 )
	   )
      )
      (values (apply 'prompt-and-read args) nil)
  )
)


;;; Presentation-definitions

(define-presentation-type t ()
  :printer ((object stream) (format stream "~S" object))
  :parser ((stream) (let ((*query-io* stream))
		         (Activating-Prompt-And-Read
			   (or default provide-default) default :read ""
			 )
		    )
	  )
)

(define-presentation-type string ()
  :printer ((object stream) (format stream "~A" object))
  :parser ((stream) (let ((*query-io* stream))
			 (let ((result (prompt-and-read :string "")))
			      (if (and (equal "" result)
				       (or default provide-default)
				  )
				  default
				  result
			      )
			 )
		    )
	  )
)

;;;Edited by RICE                  20 Feb 90  17:43
(define-presentation-type list ()
  :Parser
   ((stream)
    (let ((*query-io* stream))
         (loop for (result defaulted-p)
	       = (multiple-value-list
		   (Activating-Prompt-And-Read
		     (or default provide-default) default :read ""
		   )
		 )
	       until (or (listp result) defaulted-p)
	       do (Presentation-Parse-Error "Not a LIST.")
	       finally (return result)
	 )
    )
   )
)

;;;Edited by RICE                  20 Feb 90  17:43
(define-presentation-type cons ()
  :Parser
   ((stream)
    (let ((*query-io* stream))
         (loop for (result defaulted-p)
	       = (multiple-value-list
		   (Activating-Prompt-And-Read
		     (or default provide-default) default :read ""
		   )
		 )
	       until (or (consp result) defaulted-p)
	       do (Presentation-Parse-Error "Not a CONS.")
	       finally (return result)
	 )
    )
   )
)

(define-presentation-type Pathname
     (()
      &key (default-version :Newest)
           (default-type nil)
	   (default-name nil)
	   (dont-merge-default nil)
	   (direction :Read)
	   (format :Normal)
     )
  :Printer
    ((path stream)
     (ignore default-version default-type default-name direction
	     dont-merge-default
     )
     (ecase format
       (:Normal    (format stream "~A" (send path :string-for-printing)))
       (:Directory (format stream "~A" (send path :string-for-directory)))
       (:Dired     (format stream "~A" (send path :string-for-dired)))
       (:Editor    (format stream "~A" (send path :string-for-editor)))
      )
    )
  :Parser
    ((stream)
     (ignore format direction)
     (let ((*query-io* stream)
	   (*Add-These-Delimiters* '(#\space #\tab))
	  )
	  (loop for result
		= (catch-error
		    (if dont-merge-default
			(prompt-and-read :pathname "")
			(prompt-and-read
			  `(:Pathname :Defaults
				      ((:Name ,default-name)
				       (:Type ,default-type)
				      )
				      :Version ,default-version
			   )
			   ""
			)
		    )
		    nil
		  )
		until result
		do (Presentation-Parse-Error "Illegal pathname.")
		finally (return (if (equal result (fs:merge-pathname-defaults ""
  					           *default-pathname-defaults*
						  )
				    )
				    default
				    result
				)
			)
	  )
     )
    )
)

(defun Generic-Number-Parser
       (type default provide-default from to base stream
	&optional (prompt-type :Number)
       )
  (declare (unspecial base))
  (loop for (Number defaulted-p)
	= (let ((*query-io* stream)
		(*Add-These-Delimiters* '(#\space #\tab))
	       )
	       (multiple-value-list
		 (Activating-Prompt-And-Read
		   (or default provide-default) default
		   `(,prompt-type ,(or base *read-base*)) ""
		 )
	       )
	  )
	until (or defaulted-p
		  (and (or (not from) (>= number from))
		       (or (not to)   (<= number to))
		       (typep Number type)
		  )
	      )
	do (if (typep Number type)
	       (Presentation-Parse-Error "~S is not in the range ~A -> ~A: "
		       Number (or from "-Infinity") (or to "Infinity")
	       )
	       (Presentation-Parse-Error
		 "~S is not ~A: " (type-pretty-name type)
	       )
	   )
	finally (return Number)
  )
)

(defun generic-number-printer (type base Number stream)
  (declare (unspecial base))
  (ignore type)
  (let ((*print-base* (or base *print-base*))) (format stream "~S" number))
)

(define-presentation-type Number ((&optional from to) &key base)
  :printer ((number stream) (generic-number-printer 'number base Number stream))
  :Typep #'(lambda (x)
	     (and (numberp x)
		  (or (not from) (>= x from))
		  (or (not to) (<= x to))
	     )
	   )
  :parser  ((stream) (Generic-Number-Parser
		       'number default provide-default from to base stream
		     )
	   )
)

(define-presentation-type Integer ((&optional from to) &key base)
  :Printer
    ((number stream) (generic-number-printer 'integer base Number stream))
  :Parser
    ((stream) (Generic-Number-Parser
		'integer default provide-default from to base stream :Integer
	      )
    )
)


(define-presentation-type fixnum ((&optional from to) &key base)
  :Printer
    ((number stream) (generic-number-printer 'fixnum base Number stream))
  :Parser
    ((stream)
     (Generic-Number-Parser
       'fixnum default provide-default from to base stream :Integer
     )
    )
)

(defun print-alist-names (converted)
  (let ((names (mapcar #'first converted)))
       (if (rest names)
	   (format nil "~{~A~^, ~} or ~A"
		   (butlast names) (first (last names))
	   )
	   (format nil "~A" (first names))
       )
  )
)

(defun can-tyi-alist-p (alist)
  (let ((chars
	  (mapcar #'(lambda (entry) (aref (String (first entry)) 0)) alist)
	)
       )
       (= (length chars) (length (remove-duplicates chars)))
  )
)

(defun read-alist-member-with-tyi (alist)
  (fquery `(:Type :Tyi
	    :Choices ,(loop for entry in alist
			    for String = (String (first entry))
			    collect
			    `((,(get-value entry) ,string) ,(aref String 0))
		      )
	   )
	   ""
  )
)

(defun alist-member-1 (stream description converted alist)
  (let ((prompter
	  #'(lambda (stream)
	      (format stream "Enter ~A: " (Print-Alist-Names converted))
	    )
	)
       )
       (let ((help-function
	     #'(lambda (an-alist stream)
		 (ignore an-alist)
		 (if description
		     (format stream "~&You are being asked to enter ~A."
		       description
		     )
		     nil
		 )
		 (loop for x in alist do (accept->help-string x stream))
		 (terpri stream)
		 (funcall prompter stream)
	       )
	     )
	    )
	    (ignore stream)
	    (if (can-tyi-alist-p converted)
		(with-help-function
		  (#'(lambda (win) (funcall help-function alist win)))
		  (read-alist-member-with-tyi alist)
		)
		(completing-read converted :Help-Function help-function)
	    )
       )
  )
)

(defun stringify (x)
  (typecase x
    ((or string symbol character) (string x))
    (otherwise (format nil "~S" x))
  )
)

;;;Edited by RICE                  20 Feb 90  17:39
(define-presentation-type ticl:alist-member
       ((&key alist) &key (convert-spaces-to-dashes nil))
  :Parser ((stream)
	   (let ((converted
		   (if convert-spaces-to-dashes
		       (loop for (a . b) in alist
			collect
			  (list (substitute #\- #\space (stringify a)
					    :Test #'char-equal
				)
				(get-value b)
			  )
		       )
		       (loop for (a . b) in alist
			     collect (list (stringify a) (get-value b))
		       )
		   )
		 )
		)
	        (loop for result =
		      (alist-member-1 stream description converted alist)
		      until (member (print result) (print converted) :Key #'second)
		      do (Presentation-Parse-Error
			   "~S is not one of ~A"
			   result (print-alist-names converted)
                         )
		      finally (return result)
		)
	   )
	  )
)

(defmethod Display-Possibilities ((type (eql 'ticl:alist-member)) stream data-arglist pr-arglist)
  (ignore pr-arglist)
  (format stream "~A" (Print-Alist-Names (getf data-arglist :alist)))
)

(export 'ticl:alist-member 'ticl)

(define-presentation-type member ((&rest elements))
  :Parser ((stream)
	   (let ((converted
		   (loop for element in elements
			 collect (list (Stringify element) element)
		   )
		 )
		)
	        (loop for result =
		      (alist-member-1 stream description converted converted)
		      until (member result converted :Key #'second)
		      do (Presentation-Parse-Error
			   "~S is not one of ~A"
			   result (print-alist-names converted)
                         )
		      finally (return result)
		)
	   )
	  )
)

