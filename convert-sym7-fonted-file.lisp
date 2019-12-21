;;; -*- Mode:Common-Lisp; Mode:COMMON-LISP; Fonts:(CPTFONT HL12B HL12I hl12bi MEDFNB); Base:10; Package:USER -*-

;; This is a quick way to convert a Genera-7 fonted file.  It throws away Symbolics font information.


;; NOTE:  ** For a more complete solution to Genera7 fonts, see the G7-Compatibility subdirectory of the
;;            PUBLIC directory.  That solution fixes the READER and ZMACS to handle Genera7 fonts
;;            directly (with no "conversion" function needed) while preserving the intended font. **


(DEFCONSTANT left-parenthesis #\( )

(DEFUN convert-sym7-fonted-file (in-file out-file )
  "Translate a Symbolics Genera 7 file with character styles to the
   Explorer fonted file format.
   IN-FILE - Symbolics input file.
   OUT-FILE - Fonted Explorer output file.
   Additional information is in the source file SYS:PUBLIC;CONVERT-SYM7-FONTED-FILE.LISP

  If the error 'Package XXXX does not exist' is signalled,
 use the ’ proceed option. This is because some of
 Symbolics' character styles are in weird packages.

 After converting the file, you should edit the font list in the
 attribute line to contain valid Explorer fonts. A small amount of
 editting of the output  may be necessary to make things look
 exactly like you want them to."

;;; To run this, compile this file and execute (convert-sym7-fonted-file ...)

  (SETF in-file (TRANSLATED-PATHNAME in-file))	;translate logical pathnames
  (SETF out-file (TRANSLATED-PATHNAME out-file))

  (LET ((d-list
	  (IF (SEND in-file :wild-p)
	      (DIRECTORY (SEND in-file :new-version :newest))
	      (LIST in-file))))

    (DOLIST (sym-file d-list d-list)
      (sym1 sym-file (fs:merge-pathname-defaults out-file sym-file))))
  )

(DEFUN sym1 (in-file out-file &aux font-form font-list font-number files-attributes)

  (WITH-OPEN-FILE (in-stream in-file)
    (WITH-OPEN-FILE (out-stream (SEND out-file :translate-wild-pathname
				                out-file in-file)
				:direction :output
				:if-does-not-exist :create
				:if-exists :new-version)
      (SETF files-attributes (fs:extract-attribute-list in-stream))
      (IF files-attributes
	  (SETF font-list (GETF files-attributes :fonts))
	;; If no attribute list, make one
	(SETF font-list (PROMPT-AND-READ :read "~&~A does not have any fonts specified.~
                      ~%Please input a list of desired fonts:"
					 (STRING-UPCASE (NAMESTRING in-file))))
	(WHEN (EQUAL (SEND out-file :canonical-type)
		   :lisp)
	    (FORMAT out-stream ";;;"))                       ;Lisp files start with a comment
	(FORMAT out-stream "-*- Fonts:~A; -*-~%~%" font-list))

      (sym-skip-to-first-epsilon in-stream out-stream)	;look for the first
      (sym-skip-first-epsilon-and-stuff in-stream)	; Skip to the next ; .

      ;; Swallow (and ignore for now) Sym's initial character style forms...
      (sym-process-initial-font-descriptors in-stream)
      (LOOP
	(when (equal				;Read until eof
		(sym-look-for-epsilon in-stream out-stream)
		:eof)
	  (RETURN t))

						;read the character style form
						; to extract the font number. Then
						; write the font number to the output file
	(WHEN  (EQUAL
		(SETF font-form (READ-CHAR in-stream))	;if it's a list, put the paren
		left-parenthesis)		        ; back and READ the list.
	  (UNREAD-CHAR font-form in-stream)
	  (SETF font-form (READ in-stream)))

	;;  Extract the font number following the epsilon, if any...
	(SETF font-number
	      (COND
		((LISTP font-form) (CAR font-form))
		((DIGIT-CHAR-P font-form) (- (CHAR-INT font-form) #x30))))

	;;  Write N if  was followed by a font number N less than file's # of fonts...
	;;  This is done in lieu of rewriting the mode line using the font names from the Sym's initial
	;;  character style forms...
	(COND
	  ((AND font-number (>= font-number (LENGTH font-list))) nil)
	  (font-number (WRITE-CHAR #\epsilon out-stream)
		       (WRITE-CHAR (INT-CHAR (+ font-number #x30)) out-stream))
	  (t (WRITE-CHAR #\epsilon out-stream)
	     (WRITE-CHAR font-form out-stream)))



      ))))

(DEFUN sym-look-for-epsilon (in-stream out-stream)
  "Read characters from in-stream and write them to out-stream
   up to and excluding an ."

  (LOOP	FOR char = (READ-CHAR in-stream nil :eof)
	UNTIL (OR (EQUAL char :eof) (CHAR= char #\epsilon))
	DOING (WRITE-CHAR char out-stream)
	FINALLY (RETURN char)))

(DEFUN sym-skip-to-first-epsilon (in-stream out-stream)
  "Read characters from in-stream and write them to out-stream
   excluding the "

  (LOOP FOR char = (READ-CHAR  in-stream)
	UNTIL (CHAR= char #\epsilon)
	DO (WRITE-CHAR char out-stream)))


(DEFUN sym-skip-first-epsilon-and-stuff (in-stream)
  (sym-look-for-epsilon in-stream sys:*null-stream*)
  (SEND in-stream :untyi #\epsilon)
  )

(DEFUN sym-process-initial-font-descriptors (in-stream)
  "Swallow the initial font descriptors, building and returning a string containing the font names they
specify in the order of the font numbers assigned them."
  (LET ((array-of-font-names (MAKE-ARRAY 26)))
    (LOOP WITH font-descriptor AND font-number AND font-name
	  FOR char = (READ-CHAR in-stream)
	  WHILE (CHAR= char #\epsilon)
	  DO
	  (SETF font-descriptor (READ in-stream))
	  (SETF font-number (FIRST font-descriptor))
	  (SETF font-name (FIRST (LAST font-descriptor)))
	  (SETF (AREF array-of-font-names font-number) font-name))
    (LOOP WITH font-names-string
	  FOR font-name BEING THE ARRAY-ELEMENTS OF array-of-font-names
	  UNTIL (NULL font-name) DO
	  (SETF font-names-string
		(IF font-names-string
		    (STRING-APPEND font-names-string " " font-name)
		  font-name))
	  FINALLY (RETURN  (PRINT font-names-string)))))
