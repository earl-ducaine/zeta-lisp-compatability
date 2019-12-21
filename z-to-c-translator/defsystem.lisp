;;;;  -*- Mode:Common-Lisp; Package:USER; Base:10 -*-
;;;;
;;;;    Utilities for translating from Zetalisp to Common Lisp and checking
;;;;    for use of obsolete functions, etc.
;;;;
;;;   3/20/87 GRJ - release 3 version of Z-TO-C system.
;;;  10/31/88 DNG - initial version of Flavors-to-CLOS translator.


;; Note: if you just want to convert from Zetalisp to Common Lisp but don't 
;;	want to convert use of Flavors, don't load the file "FLAVORS-TO-CLOS".

(DEFSYSTEM Z-TO-C
  (:PATHNAME-DEFAULT #.(namestring (send sys:fdefine-file-pathname ; directory this file is in
					 :new-pathname :name nil :type nil :version nil)))
  (:PATCHABLE #.(namestring (send sys:fdefine-file-pathname ; directory this file is in
  				  :new-pathname :name nil :type nil :version nil)))
  (:MODULE MAIN ( "TRANSLATE" ))	; Translation mechanism
  (:MODULE TRANSFORMS ("Z-TO-C"		; Zetalisp to Common Lisp transformations
		       "FLAVORS-TO-CLOS" ; Flavors to CLOS transformations
		       ))
  (:COMPILE-LOAD MAIN )
  (:COMPILE-LOAD TRANSFORMS (:FASLOAD MAIN))
 )
