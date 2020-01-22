

;; Using public/probe/pprint.lisp rather then the zetalisp built-in in
;; the hope that it's more modern and portable.

(in-package :si)


;;; 75



;; 01/27/87 CLM - added font field to be used by PROBE utility
(defstruct (pp-obj (:type vector)
		   (:constructor nil)
		   (:copier nil)
		   ;; Note, this is used so that the arguments to
		   ;; various calles are unneeded. We'll have to alter
		   ;; code to provide it by hand.
		   ;; (:default-pointer print-structure)
		   )
  (type 'simple :read-only t)
  (length () :read-only t :type fixnum)
  (object () :read-only t)
  (callish () :read-only t)
  (location () :read-only t)
  (font () :type font))


;;; 96


;; This here macro is preferable to calling the keyword-parsing
;; function.
;; 01/27/87 CLM - added font field.
(defmacro make-pp-obj (&key (type ''simple) length object callish location
			 font
			 )
  `(let ((obj
	  (vector-push-extend ,type  print-structure (* 100 (size-of-pp-obj)))))
     (vector-push ,length  print-structure )
     (vector-push ,object  print-structure)
     (vector-push ,callish print-structure  )
     (vector-push ,location print-structure)
     (vector-push ,font print-structure)
     obj))
