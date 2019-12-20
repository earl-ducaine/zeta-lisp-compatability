;;; -*- Mode:Common-Lisp; Package:(G7C LISP); Fonts:(MEDFNT HL12B HL12I MEDFNT MEDFNTB); Patch-file:T; Base:10 -*-

;;; "Genera" and "Symbolics" are trademarks of Symbolics, Inc.
;;; "Explorer" is a trademark of Texas Instruments.


(DEFVAR *WARN-OF-CONVERSION-PROBLEMS-P* t
   "True => warn of Symbolics-to-Explorer conversion problems at compile time")

;;; MAINTENANCE NOTE:  To add Symbolics-to-Explorer conversion checkers, add another clause to the CASE statement
(DEFUN CONVERSION-CHECKER (form)
   (when (not *warn-of-conversion-problems-p*) (return-from conversion-checker))
   
   (case (first form)
     (scl:process-lock
      ;; for Symbolics, fourth arg must be NIL or a function; for Explorer, it must be NIL or a timeout value
      ;; Warn if we can't prove its a legal Explorer value
      (let ((arg-4 (fifth form)))
	(when (not (or (null arg-4)	   ; NIL is OK => infinite timeout
		       (numberp arg-4)))   ; any number is OK => finite timeout
	  (compiler:warn 'conversion :probable-error
			 "The fourth argument to SCL:PROCESS-LOCK is an Interlock Function while~
                        ~%the fourth argument to TICL:PROCESS-LOCK is a timeout value."))))
     );;case
   );;conversion-checker


;;; - P - P - P - P - P - P - P - P - P - P - 
(COMPILER:ADD-STYLE-CHECKER SCL:PROCESS-LOCK conversion-checker)

