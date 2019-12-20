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


(define-command (test-command :Name "Test Command" :Command-Table :Global)
		((arg0 'pathname
		       :Name "Path0"
		       :Prompt "The Pathname")
		 &key (arg1 'pathname
			    :Name "Path"
			    :Prompt "The Pathname")
		      (arg2 '((integer 1 10))
			    :Name "Copies"
			    :Prompt "Number of Copies (1-10)")
		      (arg3 '((ticl:alist-member
				:Alist (("Item 1" . a)
					("Item 2" :Value b :Documentation "the second item")))
			      :Convert-Spaces-To-Dashes T
			      :Description "a thing"
			     )
			    :Name "Things"
			    :Prompt "Pick a thing")
		)
  (print (list arg0 arg1 arg2 arg3))
)
