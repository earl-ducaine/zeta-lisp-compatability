;;; -*- Mode: COMMON-LISP; Package:USER; Base:10.; Font:CPTFONT; -*-
;;;
;;; enhanced 5-30-86 mark young
;;; hack 01/26/87 by KP Kloepper - adding specifiable type and version numbers
;;; ab 12-6-87.  Fix for PATHNAME = single filename.
;;;  3/28/88 DNG - Fix to not error on function NIL when file OPEN is aborted.
;;;  8/08/88 DNG - Fixed to work on Symbolics pathnames.
;;;  1/31/89 DNG - Fix to permit wildcarding in file name.
;;;  3/21/89 DNG - Fix to work when *STANDARD-OUTPUT* is a broadcast stream --
;;;		don't send :NAME message to streams that don't accept it.
;;;  5/2/89  ab  - Enhanced to search subdirectories.

(defvar *grep-stream-lines* (make-array 3000. :type :art-fix))	; index 0 not used ...
(defvar *grep-buffer* 1)
(defvar *grep-ever-found* NIL)
(defvar *grep-file-name-print*)
(defvar *grep-outstream* NIL)
(defvar *grep-invert-sense* NIL)
(defvar *grep-line-print* T)
(defvar *grep-print-extra* NIL)

(defvar *fynd-buffer* 1)
(defvar *fynd-outstream* NIL)
(defvar *fynd-file-type* NIL)

(deff search-file-for-string 'grep)

(defun grep (pathname string &optional &key (invert-sense nil) (type :LISP) (version :newest)
	      (search-subdirectories t) (max-depth 3)  ;; *ab* 5/3/89
	      (string2 nil) (print-extra nil) (outstream-or-file *standard-output*)
	      (line-print t) (file-name-print t))
  "Search for STRING in file(s) PATHNAME. Also search for STRING2 if non-nil.
   If PATHNAME is  a directory then all .lisp files are searched.
   TYPE is the allowable type string for the file search - default is LISP
   VERSION may be :NEWEST, :OLDEST, :WILD (for all), or a specific number.
   If SEARCH-SUBDIRECTORIES is true (the default), subdirectories of PATHNAME will also be
      searched to a maximum depth of MAX-DEPTH.  MAX-DEPTH defaults to 3, but can be NIL if
      all subdirectories should be searched.
   If INVERT-SENSE is true then the search is for NOT string1 and NOT string2, if string2 non-nil.
   If OUTSTREAM-OR-FILE is a stream - then that stream is used
      else if OUTSTREAM-OR-FILE is a file then a new version of that file is opened and used
              if not fully specified - then ~;<STRING>.GREP#> is file name,
      else if OUTSTREAM-OR-FILE is :edit-buffer then an new edit buffer is opened up and used.
   If LINE-PRINT is t then the lines containing the string are printed, plus extra lines for PRINT-EXTRA.
   If FILE-NAME-PRINT is t, then file information is printed.
   If PRINT-EXTRA is non-nil then GREP will also print PRINT-EXTRA lines before or after any line that
   contains the match string. If PRINT-EXTRA is negative, then the lines before are printed; otherwise the
   the lines after are printed. Any extra lines after are not looked at for further string matches.
   Use list value of '(:centered number-of-lines) to print centered group of NUMBER-OF-LINES before and after match.
   GREP returns t if any match was found, otherwise nil.
   "
  (declare (unspecial version))
  (let (
	lisp-names				;list of a-lisp-name's
	unwind-close
	top-pathname
	parsed-pathname
	proper-version
	)
    (setq *grep-ever-found* NIL)          ;; Set to Not(yet)-found# #
    (setq *grep-file-name-print* file-name-print)  ;; set for lower levels
    (setq *grep-invert-sense* invert-sense)
    (setq *grep-line-print* line-print)
    (setq *grep-print-extra* print-extra)
    (unwind-protect    ;; Fix up desired output stream
	(cond ((streamp outstream-or-file) )
	      ((equalp outstream-or-file :edit-buffer)
	       (setq outstream-or-file (zwei:make-file-buffer-stream (format nil "lm:printer;grep~s" *grep-buffer*)))
	       (incf *grep-buffer*))
	      (t (setq outstream-or-file (merge-pathnames outstream-or-file
							  (string-append string ".grep#>")))
	       (setq  outstream-or-file (open  outstream-or-file
						 :if-exists :new-version
						 :if-does-not-exist :create
						 :direction :output))
;;;		 (format t "~% creating file ~s" (pathname outstream-or-file))
		  (setq unwind-close t)
	      ))      ;; end of outer protected form- output established - next is clean-up forms
      (setq *grep-outstream* outstream-or-file)
      (format *grep-outstream* "~%")
      (SETQ parsed-pathname (fs:parse-pathname pathname))
      (setq proper-version (if (stringp version)
			       ;; for compatibility with older versions of this program
			       (if (equal version "#>")
				   :newest
				 (send (merge-pathnames version parsed-pathname version)
				       :version))
			     version))
      (COND ((and (SEND parsed-pathname :name)
		  (not (send parsed-pathname :name-wild-p))) 	;; Single-file
	     (setq top-pathname (send parsed-pathname :new-pathname
				      :type type
				      :version proper-version)
		   )
	     (when (equalp (pathname top-pathname) (if (send *grep-outstream* :operation-handled-p :pathname)
							(send *grep-outstream* :pathname)
						      (send *grep-outstream* :send-if-handles :name)))
		 (FERROR nil "~%Why are you trying to search the *grep-outstream* ~s ?" *grep-outstream* ))
	     (grep-search-a-file top-pathname string string2))
	    (t					;; Directory
	     (setq top-pathname (send parsed-pathname :new-pathname
				      :name (or (send parsed-pathname :name) :wild)
				      :type type :version proper-version))
	     (setq lisp-names (directory top-pathname))
	     (dolist (a-lisp-name lisp-names)
	       (when (equalp (pathname a-lisp-name) (if (send *grep-outstream* :operation-handled-p :pathname)
							(send *grep-outstream* :pathname)
						      (send *grep-outstream* :send-if-handles :name)))
		 (format t "~% ERROR: why are you trying to search the *grep-outstream* ~s ?" *grep-outstream* )
		 (return))
	       (grep-search-a-file a-lisp-name string string2)   ;; Search this file for the string
	       )
	     (WHEN (AND search-subdirectories	       ;; *ab* 5/3/89
			(OR (NULL max-depth) (AND (NUMBERP max-depth) (PLUSP max-depth))))
	       (LOOP WITH path = (PATHNAME pathname)
		     WITH dir-pathname
		     FOR el IN (fs:directory-list path)
		     WHEN (AND (PATHNAMEP (FIRST el)) (GET el :directory))
		     DO (SETF dir-pathname (SEND (FIRST el) :pathname-as-directory))
		     (SETF dir-pathname (SEND dir-pathname :new-pathname :name (SEND path :name)
			   :type (SEND path :type) :version (SEND path :version)))
		     (grep dir-pathname string :invert-sense invert-sense :type type :version version
			   :string2 string2 :print-extra print-extra :outstream-or-file outstream-or-file
			   :line-print line-print :file-name-print file-name-print
			   :max-depth (IF (NUMBERP max-depth) (1- max-depth) nil))))))
      )             ;; end of outer UnWind-Protect
    ;unwind protect forms
    (when unwind-close
;      (format t "~% closing file ~s" (pathname *grep-outstream*))
      (close  *grep-outstream*))
    *grep-ever-found*)
  )						; return if ever-found

(defun grep-search-a-file (filename string string2)
"GREP-SEARCH-A-FILE - Searches FILENAME for STRING and (if-not-nill STRING2), and prints to *Grep-outstream*"
  (let
    ( lisp-stream-input
      found
      line-cnt
      eof end
      (file-line (make-array 256. ':type 'art-string))
    )
    (unwind-protect
	    (progn
	      (setq lisp-stream-input (open filename))
	      (if (lessp (array-length *grep-stream-lines*) ; each element holds # chars in a line
			 (send lisp-stream-input :length))	; # of total chars in file
		  ;; this array is probably way too big ... but can not guarantee a smaller array.
		  (setf *grep-stream-lines* (array-grow *grep-stream-lines* (send lisp-stream-input :length))))
	      (setq line-cnt 0
		    found nil
		    eof nil)
	      (do ()
		  (eof)
		(incf line-cnt)
		(setf (aref *grep-stream-lines* line-cnt) (send lisp-stream-input :read-pointer))
		(setq file-line (multiple-value (end eof) (funcall lisp-stream-input ':line-in)))
		(when
		  (if *grep-invert-sense*
		      (not (or (string-search string file-line)
			       (and string2 (string-search string2 file-line))))
		      (or (string-search string file-line)
			  (and string2 (string-search string2 file-line))))
		  (unless found
		    (if *grep-file-name-print*
			(format *grep-outstream* "~%**** FILE: ~a~%" filename)))
		  (setq found t
			*grep-ever-found* t)
		  (when *grep-line-print*
		    (if *grep-print-extra*
			(progn
			  (setq line-cnt
				(print-lines lisp-stream-input *grep-outstream* *grep-print-extra* line-cnt *grep-stream-lines*))
			  (if (null line-cnt)
			      (setq eof t)))
			(format *grep-outstream* "~10,4,'0r: ~a~%" line-cnt file-line))
		    ))))
      (unless (null lisp-stream-input)
	(close lisp-stream-input)))
    ) )

;;; "Print-Lines" - print more than 1 line from the file

(defun print-lines (stream-input stream-output mode this-line *grep-stream-lines*)
  "Print appropriate lines and return new line-cnt."
  (let* ( start times file-line end eof)
    (format stream-output "~%")
    (if (listp mode)
	(progn
	  (file-position stream-input
			 (aref *grep-stream-lines* (setq start (max 1 (- this-line (cadr mode))))))
	  (setq times (1+ (* 2 (cadr mode))))
	  )
    ;else
	(file-position stream-input
		       (aref *grep-stream-lines* (setq start (if (plusp mode)
							    this-line	; current line
						      ;else rewind "times" lines; mode is negative
							  (max 0 (+ this-line mode))))))
	(setq times (1+ (abs mode))) ; will print same number of lines even if at beginning of file
	)
    (dotimes (i times)
      (setf (aref *grep-stream-lines* start) (send stream-input :read-pointer))
      (setq file-line (multiple-value (end eof) (funcall stream-input ':line-in)))
      (if eof
	  (return-from print-lines nil))
      (format stream-output "~10,4,'0r: ~a~%" start file-line )
      (incf start))
    (setf (aref *grep-stream-lines* start) (send stream-input :read-pointer))
    (- start 1)					; return new line cnt
    ))

;;; and Now for FYND - to find a file in a directory.

(defun fynd ( dir-name fname &optional &key (invert-sense nil) (type "*")
	     (fname2 nil) (outf *standard-output*) )
" FYND - Finds a file in DIR-NAME with the substring FNAME or with FNAME2,
         INVERT-SENSE selects all files that Do-NOT have FNAME or FNAME2,
         TYPE is file.TYPE for match - default is * (all types)
         If OUTF is a stream - then that stream is used (*Standard-Output* is Default),
            or if OUTF is a file then a new version of that file is opened and used,
               and if not fully specified - then ~;<FNAME>.FYND#> is file name,
            or if OUTF is :edit-buffer then an new edit buffer is opened up and used."
  (let
    ( (hostname (host-namestring dir-name))
      (directry (directory-namestring dir-name))
      (unwind-close NIL)     ;; Flag to close locally opened output stream
      (print-what NIL)       ;; Flag
    )
;;; Fix up output file/stream
    ;;(format t "~% Host is ~A and Directry is ~A" hostname directry)
    (cond ((streamp outf) )
	  ((equalp outf :edit-buffer)
	   (setq outf (zwei:make-file-buffer-stream (format nil "lm:printer;fynd~s" *fynd-buffer*)))
	   (incf *grep-buffer*)
	   (setq print-what t))
	  (t (setq outf (merge-pathnames outf
					 (string-append fname ".fynd#>")))
	     (setq  outf (open  outf
				:if-exists :new-version
				:if-does-not-exist :create
				:direction :output))
	     (setq unwind-close t)   ;; so it is closed at end
	     (setq print-what t)
	     ))      ;; end of outer protected form- output established - next is clean-up forms
    (setq *fynd-outstream* outf)
    (setq *fynd-file-type* type)
    (if print-what
	(let ()
	  (format *fynd-outstream* "~%;;; Fynd \"~A\" " fname)
	  (if fname2
	      (format *fynd-outstream* "or \"~A\" " fname2) )
	  (format *fynd-outstream* " in ~A~2%" dir-name) ) )
    ;;(format t "~% OUTF is ~A~%" outf)

;;; Now look for filenames in this directory
    (format *fynd-outstream* "~%")
    (fynd-files fname fname2 hostname directry)
    (fynd-below fname fname2 hostname directry)

    (if unwind-close
	(close *fynd-outstream*))
  ) )   ;; End of FYND

;;; FYND-BELOW - given a host and directory name - check its files

(defun fynd-below (str1 str2 host dirctry)
" FYND-BELOW - find matching files in sub-directories of HOST:DIRCTRY;"
  (let
    ( (dirlst (directory (pathname (string-append host dirctry "*.directory#>")))) )
    (dolist (dirfyl dirlst)
      (let*
	( (fname (namestring dirfyl))
	  (host (host-namestring dirfyl))
	  (dirnam (string-append
		    (substring (directory-namestring fname) 0
			       (string-search-char #\; (directory-namestring fname)))
		    "." (substring (file-namestring fname) 0
				   (string-search-char #\. (file-namestring fname)))
		    ";")) )
	(fynd-files str1 str2 host dirnam)
	(fynd-below str1 str2 host dirnam)
      ) )

  ) )   ;; End of FYND-BELOW

;;; FYND-FILES - look thru results of directory for fname or fname2

(defun fynd-files (str1 str2 host dirctry)
" FYND-FILES - looks for STR1 or STR2 in DIRLST (a directory list)"
  (let (
	 (dirlist (directory (pathname (string-append host dirctry
						      "*." *fynd-file-type* "*#>"))))
	 fname )
    ;;(format t " Str1 is ~A str2 is ~A~%" str1 str2)
    ;;(format t " DirList is ~A~%" dirlist)
    (dolist (fyle dirlist)
      (setf fname (substring (file-namestring fyle) 0
			     (string-search-char #\. (file-namestring fyle))))
      ;;(format t "    Element is ~A - ~A~%" fyle fname)
      (if (or
	    (and str1 (string-search str1 fname))
	    (and str2 (string-search str2 fname)) )
	  (format *fynd-outstream* "   ~A~%" fyle) ) )
  ) )   ;; End of FYND-FILES

;;; End of File - GREP.LISP
