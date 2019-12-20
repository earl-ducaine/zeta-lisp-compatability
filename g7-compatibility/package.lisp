;;; -*- Mode:Common-Lisp; Package:(USER); Fonts:(MEDFNT HL12B HL12I MEDFNT MEDFNTB); Patch-file:T; Base:10 -*-


;;; Patch by JPR to fix up after-intern-daemon.

sys:
(Defmacro SHORTEST-NAME-OR-NICKNAME (pkg)
    `(LET ((shortest-name (PACK-NAME ,pkg)))
       (DOLIST (nick (PACK-NICKNAMES ,pkg) shortest-name)
	 (IF (AND
	       (NOT (EQUAL nick ""))
	       (< (LENGTH nick) (LENGTH shortest-name)))
	     (SETQ shortest-name nick)))))

sys:
(Defmacro PARSE-PACKAGE-ARGUMENT (pkg)
;; expands into code which attempts to produce a package object from the argument <pkg>
;; and default to *PACKAGE* if omitted.
;; Most package functions, e.g. intern, expect a package object as the second argument.
    `(COND ((NULL ,pkg) *PACKAGE*)
	   ((FIND-PACKAGE ,pkg))   ;; at this point, <pkg> should be a package object
	   (T (PACKAGE-DOES-NOT-EXIST-ERROR  ,pkg))))

sys:
(Defun MAKE-PACKAGE (name &KEY
		     nicknames
		     (use '("LISP" "TICL"))
		     (size 200)
		     shadow
		     export
		     prefix-name 
		     auto-export-p 
		     import 
		     (area SYS:NR-SYM)
		     shadowing-import 
		     plist
		     store-function
		     after-intern-daemon
		     )
  
  "Creates and returns a package object named <name>.  The name must be 
distinct from the names and nicknames for all existing packages or an 
error will result. The keyword arguments are as follows:
1) :USE specifies a list of names of packages for this one to use. 
   (see USE-PACKAGE).
2) :NICKNAMES specifies a list of nicknames for this package.  The nicknames
   must be distinct from the names and nicknames of all existing packages.
3) :SHADOW specifies a list of names of symbols to shadow in this package
   (see SHADOW).
4) :EXPORT specifies a list of names of symbols to export in this package
   (see EXPORT).
5) :IMPORT specifies a list of symbols to import in this package
   (see IMPORT).
6) :SHADOWING-IMPORT specifies a list of symbols to import in this package,
    overriding any name conflicts (see SHADOWING-IMPORT).
7) :PREFIX-NAME specifies the name to be used when printing the symbols.  
   This MUST be a name or nickname of the package.
8) :AUTO-EXPORT-P non-NIL specifies that all symbols placed in this package
   should be exported automatically at that time.
9) :SIZE specifies the number of symbols to allocate space for initially."
 
  (LET ((pkg (INTERNAL-MAKE-PACKAGE  :all-packages-pointer *PACKAGE-HASH-TABLE*
				     :number-of-symbols 0
				     :max-number-of-symbols size))
	success)
    (UNWIND-PROTECT      ;; set unwind-protect in case we must kill package object just created
	(PROGN
	  (WITHOUT-INTERRUPTS 
	    (SETF (PACK-NAME pkg) (ENTER-STRING-INTO-TABLE name pkg))
	    (SETF (PACK-NICKNAMES pkg) 
		  (LET (new-list)
		    (DOLIST (item (IF (LISTP nicknames ) nicknames (LIST nicknames)) (NREVERSE (the list new-list)))
		      (PUSH (ENTER-STRING-INTO-TABLE item pkg) new-list)))))
	  ;; determine a prefix name -- used by the printer/dumper/loader
	  (LET ((prefix  
		  (IF (AND 
			prefix-name
			(OR (MEMBER prefix-name (PACK-NICKNAMES pkg) :test #'string=)
			    (STRING= prefix-name name)))
		      prefix-name
		      (SHORTEST-NAME-OR-NICKNAME pkg))))
			
	  (SETF (PACK-PREFIX-NAME pkg) prefix
		(PACK-SYMBOL-TABLE pkg) (MAKE-ARRAY (LIST (GET-GOOD-PACKAGE-SIZE size) 2) :AREA pkg-area)
		(PACK-PLIST pkg) plist
		(PACK-INTERN-AREA pkg) (COND ((AND (NUMBERP area) 
						   (<= 0 area (SYMBOL-VALUE (CAR (LAST area-list))))) 
					      area)
					     ((MEMBER area area-list :test #'EQ) 
					      (SYMBOL-VALUE area))
					     ((SYMBOLP area)  
					      (MAKE-AREA :NAME area :REPRESENTATION :structure) (SYMBOL-VALUE area))
					     (t (ERROR t "area keyword argument ~s is invalid" area))))
	  )
	  (IF (AND auto-export-p (NOT after-intern-daemon))
	      (SETF (PACK-AUTO-EXPORT-P pkg) auto-export-p
		    (PACK-AFTER-INTERN-DAEMON pkg) 'externalize-all-symbols)
	      (WHEN auto-export-p
		(ERROR t "in making package ~a--autoexport slot uses after-intern-daemon slot" name)))

	  (WHEN store-function
	    (SETF (PACK-STORE-FUNCTION pkg) store-function))
	  (WHEN after-intern-daemon
	    (SETF (PACK-AFTER-INTERN-DAEMON pkg) after-intern-daemon))

	  (WHEN shadow (SHADOW shadow pkg))
	  (WHEN shadowing-import (SHADOWING-IMPORT shadowing-import pkg))
	  (WHEN import (IMPORT import pkg))
	  (WHEN export
	    (dolist (x (if (listp export) export (list export)))
	      (EXPORT (if (stringp x) (intern x pkg) x) pkg)))	
	  (WHEN use (USE-PACKAGE use pkg))
	  (SETQ success t)
	  pkg)
      (UNLESS success
	(KILL-PACKAGE pkg))
      )))

sys:
(Defun ALTER-PACKAGE (name &KEY nicknames
		      (use '("LISP" "TICL"))
		      size
		      shadow 
		      export 
		      prefix-name 
		      auto-export-p
		      import 
		      shadowing-import 
		      properties
		      after-intern-daemon)
  (DECLARE (IGNORE size))
  (LET ((pkg (PARSE-PACKAGE-ARGUMENT name)))
    (UNLESS (LISTP nicknames) (SETQ nicknames (LIST nicknames)))
    (RENAME-PACKAGE pkg (PACK-NAME pkg) nicknames)
    (UNLESS (OR (NULL prefix-name) (STRING= prefix-name name) (MEMBER prefix-name nicknames :TEST #'STRING=))
      (ERROR nil "The prefix name ~A is not a name or nickname of the package." prefix-name))
    (SETF (PACK-PREFIX-NAME pkg) (OR prefix-name (SHORTEST-NAME-OR-NICKNAME pkg)))
    (LOOP for (prop val) on properties by 'cddr
	  do (SETF (GETF (PACK-PLIST pkg) prop) val))
    (WHEN shadow (SHADOW shadow pkg))
    (WHEN shadowing-import (SHADOWING-IMPORT shadowing-import pkg))
    (WHEN export
	    (dolist (x (if (listp export) export (list export)))
	      (EXPORT (if (stringp x) (intern x pkg) x) pkg)))	
    (LET ((desired-use (IF (LISTP use)
			   (MAPCAR #'PKG-FIND-PACKAGE use)  ;;8/26/88 clm
			   (LIST (PKG-FIND-PACKAGE use)))))
      (DOLIST (elt (PACK-USE-LIST pkg))
	(UNLESS (MEMBER elt desired-use)
	  (UNUSE-PACKAGE elt pkg)))
      (USE-PACKAGE desired-use pkg))
    (WHEN import (IMPORT import pkg))
    (WHEN after-intern-daemon
      (SETF (PACK-AFTER-INTERN-DAEMON pkg) after-intern-daemon))
    (COND (auto-export-p 
	   (SETF (PACK-AFTER-INTERN-DAEMON pkg) 'externalize-all-symbols) ;;; JPR.
	   (SETF (PACK-AUTO-EXPORT-P pkg) T))
	  (T
	   (SETF (PACK-AFTER-INTERN-DAEMON pkg) nil)
	   (SETF (PACK-AUTO-EXPORT-P pkg) nil)))
    pkg))


;;; "Genera" and "Symbolics" are trademarks of Symbolics, Inc.
;;; "Explorer" is a trademark of Texas Instruments.

;; add nicknames of USER
(unless (find-package 'cl-user)
  (DEFPACKAGE "USER" (:nicknames "CL-USER" "ZL-USER" "COMMON-LISP-USER" "ZETALISP-USER")))

(DEFPACKAGE "G7-COMPATIBILITY" (:nicknames "G7C") (:use "LISP") (:prefix-name "G7C"))

(DEFPACKAGE "SYMBOLICS-COMMON-LISP" (:use "LISP" "TICL") (:nicknames "SCL")
	      (:prefix-name "SCL") (:AUTO-EXPORT-P t)
	      (:import zlc: zlc: zlc: zlc:deposit-byte zlc:load-byte)
	      ;;; Defmethod, defflavor, ticl:lexpr-send: JPR. 
	      (:shadow defmethod defflavor string-compare string-search-set string-search-not-set
		       ticl:lexpr-send)
	      ;(:after-intern-daemon sys:externalize-all-symbols)
	      (:auto-export-p t))

(DEFPACKAGE "DYNAMIC-WINDOW" (:use "LISP" "TICL") (:nicknames "DW")
		 (:prefix-name "DW")
		 (:after-intern-daemon nil)
		 (:auto-export-p t))

(DEFPACKAGE "GRAPHICS" (:use "LISP" "TICL")
	      (:after-intern-daemon nil)
	      (:AUTO-EXPORT-P t))

(DEFPACKAGE "MOUSE" (:use "LISP" "TICL")
	      (:after-intern-daemon nil)
	      (:AUTO-EXPORT-P t))

(DEFPACKAGE "FLAVOR" (:use "SCL")
	      (:after-intern-daemon nil)
	      (:auto-export-p t))


(import '(w:font-baseline w:font-blinker-height w:font-blinker-width
          w:font-char-height w:font-char-width w:font-char-width-table
	  w:font-chars-exist-table w:font-indexing-table w:font-left-kern-table
	  w:font-name w:font-raster-height w:font-raster-width) 'zl)

(export '(zl:font-baseline zl:font-blinker-height zl:font-blinker-width
          zl:font-char-height zl:font-char-width zl:font-char-width-table
	  zl:font-chars-exist-table zl:font-indexing-table zl:font-left-kern-table
	  zl:font-name zl:font-raster-height zl:font-raster-width) 'zl)
