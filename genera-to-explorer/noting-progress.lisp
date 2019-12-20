;;; -*- Mode:Common-Lisp; Package:TV; Base:10 -*-

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

;;; The following code written by JPR on 2/5/89


(defvar *noting-bar-thickness* 2)
(defvar *noting-font* fonts:tvfont)
(defvar *current-progress-note* nil)
(defvar *active-noters* nil)
(defvar *note-separation* 1)
(defvar *notation-fraction* 0.7)
(defvar *default-offset-note-line-from-font-baseline* 2)
(defvar *noting-sheet* who-line-documentation-window)
(defvar *noting-color* w:blue-green)

(defflavor note
	   (name font thickness process (sheet *noting-sheet*)
	    (offset *default-offset-note-line-from-font-baseline*)
	    (bottom-of-sheet-offset nil)
	   )
	   ()
  :Initable-instance-variables
  :Gettable-instance-variables
)

(defmethod (note :item-height) ()
  (+ thickness offset (font-char-height font))
)

(defmethod (note :print-self) (stream &rest ignore)
  (format stream "#<Note ~A>" name)
)

(defmethod (note :find-offset) (max-height)
  (if bottom-of-sheet-offset
      bottom-of-sheet-offset
      (let ((max (apply #'max
			0
			(mapcar #'(lambda (x)
				    (or (+ (send x :bottom-of-sheet-offset)
					   (send x :item-height)
					   *note-separation*
					)
					0
				    )
				  )
				  (remove self *active-noters*)
			)
		 )
	    )
	   )
	   (setq bottom-of-sheet-offset
		 (if (> (+ (send self :item-height) max) max-height)
		     0
		     max
		 )
	   )
	   bottom-of-sheet-offset
      )
  )
)

(defmacro noting-progress
	  ((name &optional
		 (variable '*current-progress-note*)
		 (process 'si:current-process)
	   )
	   &body body
	  )
"Make a progress note named NAME for the duration of BODY."
 `(let ((,variable (make-note-instance ,name ,process)
        )
       )
       (unwind-protect (progn (push ,variable *active-noters*) ,@body)
	 (clear-note-area ,variable)
	 (setq *active-noters* (remove ,variable *active-noters*))
       )
  )
)

(defun make-note-instance (name process)
  (make-instance 'note
		 :name name
		 :font *noting-font*
		 :process process
		 :thickness *noting-bar-thickness*
		 :bottom-of-sheet-offset 0
  )
)

(defun find-place-for-note (note)
  (declare (values sheet left top right bottom))
  (let ((sheet (send note :sheet)))
       (multiple-value-bind (width height) (send sheet :inside-size)
	 (values sheet
		 (floor (* width *notation-fraction*))
		 (- height (send note :item-height)
		    (send note :find-offset height)
		 )
		 width
		 (- height (send note :find-offset height))
	 )
       )
  )
)

(defun clear-note-area (note)
  (if (not (si:mx-p))
      (multiple-value-bind (sheet left top right bottom)
	  (find-place-for-note note)
	(prepare-sheet (sheet)
	  (%draw-rectangle (- right left) (- bottom top) left top
			      (sheet-erase-aluf sheet) sheet
	  )
	)
       sheet
      )
      nil
  )
)

(defun note-progress
       (numerator &optional (denominator 1) (note *current-progress-note*))
  (if (not (si:mx-p))
      (let ((string (format nil "~A: ~D%" (send note :name)
		      (floor (* 100.0 (abs (min 1 (/ numerator denominator)))))
		    )
	    )
	    (font (send note :font))
	   )
	   (multiple-value-bind (sheet left top right bottom)
	       (find-place-for-note note)
	     (ignore top)
	     (let ((real-string
		     (loop for index from (length string) downto 0
			   until (<= (sheet-string-length
				       sheet string 0 index nil font
				     )
				     (- right left)
				 )
			   do nil
			   finally (return (subseq string 0 index))
		     )
		   )
		  )
		  (clear-note-area note)
		  ;(string start-x start-y x-limit y-limit font alu &optional
		  ;(start 0) end multi-line-line-height color)
		  (send sheet :string-out-explicit
			real-string
			left
			(- bottom (font-baseline (send note :font))
			   (send note :thickness) (send note :offset)
			)
			right
			(- bottom (send note :thickness))
			(send note :font)
			(sheet-char-aluf sheet)
			0
			nil
			nil
			(if (color-system-p sheet) *noting-color* nil)
		  )
		  (prepare-sheet (sheet)
		    (let ((end (+ left (round (* (- right left)
						 (abs (min 1 (/ numerator
								denominator
							     )
						      )
						 )
					      )
				       )
			       )
			  )
			 )
			 (loop for delta from 1 to (send note :thickness) do
			       (%draw-line left (- bottom delta) end
					   (- bottom delta)
					   (sheet-char-aluf sheet) t sheet
			       )
			 )
		    )
		  )
	     )
	   )
      )
      nil
  )
)

;-------------------------------------------------------------------------------

(defvar *run-light-char* #\$)
(defvar *run-light-font* fonts:mouse)

(defun set-run-light (index on-p)
  (if (sys:mx-p)
      nil
      (let ((char-width (tv:font-char-width *run-light-font*)))
	   (multiple-value-bind (width)
	       (send tv:who-line-documentation-window :size)
	     (send tv:who-line-documentation-window :string-out-explicit
	       (string *run-light-char*) (- width (* index (+ 1 char-width)))
	       (sheet-inside-top tv:who-line-documentation-window) nil nil *run-light-font*
	       (if (tv:color-system-p tv:who-line-documentation-window)
		   (if on-p tv:alu-add  tv:alu-sub)
		   tv:alu-xor))))))
