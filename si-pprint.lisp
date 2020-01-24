

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




;; (defun pprin1 (object &optional (*standard-output* *standard-output*))
;;   "prettily outputs the object to the stream slashifying special characters."
;;   ;(setup-printer-state)
;;   (let ((*print-escape* t))
;;     (print-circle (output-pretty-object object))))

;; (defun pprinc (object &optional (*standard-output* *standard-output*))
;;   "prettily outputs the object to the stream without slashifying."
;;   ;(setup-printer-state)
;;   (let ((*print-escape* ()))
;;     (print-circle(output-pretty-object object))))


(setf *print-case* :downcase)

(defun pprint-alt (object &optional stream terpri-p)
  "Prettily output object."
  (let ((*print-pretty* t)
        (*print-escape* t)
        (stream (sb-impl::out-stream-from-designator stream)))
    (when terpri-p
      (terpri stream))
    ;; (sb-impl::output-object object stream)
    (write object :stream stream)
    )
  (values))

(defvar *pprint-notify-fun* nil)

;; AB for PHD 6/19/87 Fixed grind-top-level so it can print with
;; *print-circle* bound to T.  SPR 5557.

;; Removed: grind-untyo-p, grind-displaced grind-format
(defun grind-top-level (exp &optional (grind-width nil)
			      grind-real-io
			      (terpri-p t)
			      pprint-notify-fun
			      (loc (cons exp nil))
			      (initial-indentation 10))
  "pretty-print the list exp on stream grind-real-io. note that it is
   an obsolete function, pprint should be used instead. grind-width is
   the width to fit within; nil is the default, meaning try to find
   out the stream's width. terpri-p non-nil says go to a fresh line
   before printing. *pprint-notify-fun*, if non-nil, is called for
   each cons-cell processed. use this to keep records of how list
   structure was traversed during printing. loc is the location where
   exp was found, for passing to *pprint-notify-fun*.
   initial-indentation is the horizontal indent to use for the first
   line. Additional lines are indented relative to the first."
  (let ((*print-case* :downcase)
	(*print-pretty* t)
	(*pprint-notify-fun* (or pprint-notify-fun *pprint-notify-fun*))
	(stream (or grind-real-io t)))
    (when terpri-p
      (terpri stream))
    (when (or *pprint-notify-fun* pprint-notify-fun)
      (setf stream (sb-pretty::make-pretty-stream stream))
      (setf (slot-value stream 'sb-pretty::char-out-oneshot-hook)
    	    pprint-notify-fun))
    (pprint-indent :block 10 stream)
    (write exp
	   :stream stream
	   :case :downcase
	   :right-margin grind-width
	   )
	   (values)))
