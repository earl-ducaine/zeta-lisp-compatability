;;; -*- Mode:Common-Lisp; Package:USER; Base:10 -*-

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

;;;Edited by RICE                  1 Feb 90  11:13
(make-system :g7c :noconfirm :nowarn :silent)

;;;Edited by RICE                  1 Feb 90  11:13
;;;Edited by RICE                  1 Feb 90  11:15
;;;Edited by RICE                  1 Feb 90  13:40
(defvar *general-module-files*
	'(("WITHOUT-ABORTS") ("UTILS") ("NOTING-PROGRESS")
	  ("SERVER-DEFINITIONS") ("PATCH-SYSTEM")
	  ("CHARACTER-MAPPINGS")
	 )
)

(defsystem genera-to-explorer
  (:Name "New G7 compatibility stuff.")
  (:pathname-default "GENERA-TO-EXPLORER:GENERA-TO-EXPLORER;")
  (:output-version :newest)
  (:module packages (("PACKAGE-DEFINITIONS")))
  (:module timer-queues (("TIMER-QUEUES")))
  (:Module macros (("MACROS")))
  (:module variables (("VARIABLES")))
  (:Module commands-etc (("COMMAND-STUFF") ("TYPEIN-MODES") ("DEFCOMMAND")))
  (:Module command-definitions (("COMMAND-DEFINITIONS")))
  (:module presentation (("PRESENTATION-STUFF")))
  (:module presentation-definitions (("PRESENTATION-DEFINITIONS")))
  (:module defsystem-interface (("DEFSYSTEM-INTERFACE")))
  (:Module general
	   #.(if (type-specifier-p 'w:live-listener)
		 (cons '("LIVE-LISTENER-SPECIFIC") *general-module-files*)
		 *general-module-files*
	     )
  )
  (:compile-load packages)
  (:Compile-Load macros (:fasload packages))
  (:compile-load variables (:fasload packages))
  (:compile-load defsystem-interface (:fasload packages))
  (:compile-load-init timer-queues (macros) (:Fasload packages macros))
  (:compile-load-init commands-etc (macros) (:Fasload packages macros))
  (:compile-load-init presentation (macros) (:Fasload packages macros))
  (:compile-load-init presentation-definitions (macros presentation)
		      (:fasload packages macros presentation commands-etc)
  )
  (:compile-load-init command-definitions (macros commands-etc)
		      (:fasload packages macros commands-etc)
  )
  (:Compile-Load-init general (macros) (:fasload packages macros))
)
