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

(defun evaluate-timer-queue-entry (entry)
  (let ((repeat (timer-queue-entry-repeat entry))
	(delete-p nil)
       )
       (incf (timer-queue-entry-time entry) (second repeat))
       (case (first repeat)
	 (:forever nil)
	 (:once (setq delete-p t))
	 (otherwise ;;; n-times.
	  (decf (first repeat))
	  (if (equal 0 (first repeat))
	      (setq delete-p t)
	      nil
	  )
	 )
       )
       (apply (timer-queue-entry-function entry) (timer-queue-entry-args entry))
       (if delete-p
	   (setf (timer-queue-queue (timer-queue-entry-queue entry))
		 (remove entry (timer-queue-queue
				 (timer-queue-entry-queue entry)
			       )
		 )
	   )
       )
  )
)

(defun find-next-timer-queue-start-time (timer)
  (without-interrupts
    (setf (timer-queue-next-start-time timer) nil)
    (setf (timer-queue-next-active-entry timer) nil)
    (loop for entry in (timer-queue-queue timer)
	  for time = (timer-queue-entry-time entry)
	  when (or (not (timer-queue-next-start-time timer))
		   (< time (timer-queue-next-start-time timer))
	       )
	  do (setf (timer-queue-next-start-time timer) time)
	     (setf (timer-queue-next-active-entry timer) entry)
    )
  )
  timer
)

(defun timer-queue-processing-function (timer)
  (loop do
	(si:process-wait "Wait for entry"
			 #'(lambda ()
			     (and (timer-queue-queue timer)
				  (timer-queue-next-start-time timer)
				  (> (time:get-universal-time)
				     (timer-queue-next-start-time timer)
				  )
			     )
			   )
        )
	(evaluate-timer-queue-entry (timer-queue-next-active-entry timer))
	(find-next-timer-queue-start-time timer)
  )
)

(defun remove-timer-queue-entry (id &optional (timer *timer*))
"Removes a timer queue entry from timer Timer which has id ID.
This id is the value returned by sys:add-timer-queue-entry.
"
  (declare (special *timer*))
  (without-interrupts
    (setf (timer-queue-queue timer)
	  (remove-if #'(lambda (x) (equal id (timer-queue-entry-id x)))
		     (timer-queue-queue timer)
	  )
    )
    (find-next-timer-queue-start-time timer)
  )
)

(defun remove-timer-queue-entry-named (name &optional (timer *timer*))
"Removes a timer queue entry from timer Timer which has name NAME.
This name is the value specified for the NAME argument to
sys:add-timer-queue-entry, not the ID returned by sys:add-timer-queue-entry.
"
  (declare (special *timer*))
  (without-interrupts
    (setf (timer-queue-queue timer)
	  (remove-if #'(lambda (x) (equal name (timer-queue-entry-name x)))
		     (timer-queue-queue timer)
	  )
    )
    (find-next-timer-queue-start-time timer)
  )
)


(defun add-timer-queue-entry (time repeat name function &rest args)
"
Adds a timer queue entry.
  Time - can be of the form
         <<Absolute universal time>>
         (:absolute <<Absolute universal time>>)
         (:relative <<seconds relative to now>>)
  Repeat - can be of the form
         :Once - do this only once
         (:forever <<repeat interval in seconds>>)
         (<<number of repeats>> <<repeat interval in seconds>>)
  Name - is the name of the entry
  Function - is the function to call at the appropriate time
  Args - are the args to call Function with when it is called
Returns the internal ID of the timer queue entry, suitable as an argument to
sys:remove-timer-queue-entry.
"
  (declare (values timer-queue-entry-id))
  (declare (special *timer*))
  (apply 'add-timer-queue-entry-to-timer *timer* time repeat name function args)
)

(defun add-timer-queue-entry-to-timer
       (timer time repeat name function &rest args)
  (assert (or (eq :once repeat)
	      (and (consp repeat)
		   (integerp (second repeat))
		   (or (integerp (first repeat))
		       (equal :forever (first repeat))
		   )
	      )
	  )
  )
  (let ((actual-time
	  (etypecase time
	    (cons
	     (ecase (first time)
	       (:absolute (second time))
	       (:relative (+ (second time) (time:get-universal-time)))
	     )
	    )
	    (fixnum time)
	  )
	)
	(actual-repeat
	  (etypecase repeat
	    (cons repeat)
	    (keyword (list repeat 0))
	  )
	)
	(id (gensym "TIMER-"))
       )
       (gensym "G") ;;; Zeroise gensym.
       (let ((entry (make-timer-queue-entry
		      :name name :time actual-time :repeat actual-repeat
		      :function function :args args :id id :queue timer
		    )
	     )
	    )
	    (without-interrupts
	      (push entry (timer-queue-queue timer))
	      (find-next-timer-queue-start-time timer)
	    )
       )
       id
  )
)

(define-time-queue "System" *timer*)


;;; Tests:
;(describe *timer*)
;(add-timer-queue-entry '(:relative 10) :once "Hello" 'print :Hello tv:selected-window)
;(add-timer-queue-entry '(:relative 10) '(3 5) "Hello" 'print :Hello tv:selected-window)
;(add-timer-queue-entry '(:relative 0) '(:forever 5) "Hello" 'print :Hello tv:selected-window)
;(remove-timer-queue-entry-named "Hello")