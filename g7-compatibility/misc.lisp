;; -*- Mode:Common-Lisp; Package:(G7C LISP); Fonts:(MEDFNT HL12B HL12I MEDFNT MEDFNTB); Patch-file:T; Base:10 -*-
;;; "Genera" and "Symbolics" are trademarks of Symbolics, Inc.
;;; "Explorer" is a trademark of Texas Instruments.

;;; Functions with Different Names

(ticl:deff tv:pop-up-notification 'tv:pop-up-notify)

;;; - A - A - A - A - A - A - A - A - A - A -
(DEFUN TV:ADD-FUNCTION-KEY (char function docs &rest options)
  "Installs the invoking of a function on a TERMINAL key combination."
  ;; This may not work in all cases.  Symbolics allows more documentation
  ;; options than we do and more options that we do
  (w:add-terminal-key char function docs options))

(DEFUN TV:ADD-SELECT-KEY (char flavor name &optional (create-p t) ignore)
  "Installs the selection of a program onto a SYSTEM key combination."
  (w:add-system-key char flavor (format nil "Invokes the ~a." name) create-p))

(DEFUN TV:ADD-TO-SYSTEM-MENU-CREATE-MENU (name flavor documentation &optional ignore)
  "Adds a new window type to the w:default-window-types-item-list. This window type
will show up in the system menu create option."
  (w:add-window-type name flavor documentation))

(proclaim '(declaration sys:array-register))
(proclaim '(declaration sys:array-register-1d))


;;; - B - B - B - B - B - B - B - B - B - B -
(DEFUN SI:BACKTRANSLATE-FONT (font)
   (let ((style (first (rassoc (list (tv:font-name font)) 
			       *character-style-explorer-font-alist* :test #'equal))))
     (scl:make-character-style (first  style) (second style) (third  style))))


(DEFUN SYS:%BEEP (half-period duration)
  (w:simple-beep (truncate 1000000. (* half-period 2)) duration))
  

;;; - C - C - C - C - C - C - C - C - C - C -
(proclaim '(inline char-to-ascii))
(DEFUN SCL:CHAR-TO-ASCII (ch)
   (if (= (char-code ch) (char-code #\RETURN))
       (char-code #\LINEFEED)
       (char-code ch)))


(proclaim '(inline char-fat-p))
(DEFUN SCL:CHAR-FAT-P (char)
   "returns true if CHAR has either anon-zero font or non-zero bits fields"
   (or (zerop (char-font char))
       (zerop (char-bits char))))


(proclaim '(inline char-flipcase))
(DEFUN SCL:CHAR-FLIP-CASE (char)
   "If CHAR is uppercase, make it lowercase, and visa versa.  Otherwise, return CHAR."
   (cond ((upper-case-p char) (char-downcase char))
	 ((lower-case-p char) (char-upcase char))
	 (t char)))


(DEFUN SCL:CHAR-MOUSE-EQUAL (char1 char2)
   "returns true if mouse characters CHAR1 and CHAR2 are equal"
   (and (= (ticl:char-mouse-button char1) (ticl:char-mouse-button char2))
	(= (char-code char1)              (char-code char2))))


(DEFUN SYS:CHAR-SUBINDEX (char)
   "dummy version of Symbolics function"
   (declare (values 0))
   (check-type char character)
   (char-code char))


(DEFUN SYS:CHAR-STYLE (char)
   "dummy version of Symbolics function"
   (declare (values nil.nil.nil))
   (check-type char character)
   (scl:make-character-style))


(DEFUN CHARACTER-STYLE-EQL (style1 style2)
   "returns true if family, face, size, and color components of STYLE1 and STYLE2 are EQUAL
where missing components off the end are treated as NIL"
   (if (character-style-p style1)
       (if (character-style-p style2)
	   (equal style1 style2)	                   ; STRUCTURE vs STRUCTURE
	   (and (eql (cs-family style1) (first  style2))   ; STRUCTURE vs LIST
		(eql (cs-face   style1) (second style2))
		(eql (cs-size   style1) (third  style2))
		(eql (cs-color  style1) (fourth style2))))
       (if (character-style-p style2)
	   (and (eql (first  style2) (cs-family style1))   ; LIST vs STRUCTURE
		(eql (second style2) (cs-face   style1))
		(eql (third  style2) (cs-size   style1))
		(eql (fourth style2) (cs-color  style1)))
	   (and (eql (first  style1) (first  style2))	   ; LIST vs LIST
		(eql (second style1) (second style2))
		(eql (third  style1) (third  style2))
		(eql (fourth style1) (fourth style2))))))                                          
	      
;;; - D - D - D - D - D - D - D - D - D - D -
;;; CONVERSION NOTE:  The Explorer stores the structure symbol identifying a DEFSTRUCT in the
;;; structure's array leader.  Therefore, the first element of the structure vector holds the first
;;; slot value.  However, Symbolics stores the structure symbol in the first vector element so that
;;; the first slot value is in the second vector element.
;;;
;;; During conversion, this difference never causes a problem unless someone does something illegal
;;; such as accessing the structure as a vector using an AREF rather than using the defined slot
;;; accessors.
;;;
;;; You can force the Explorer to create AREF-compatible structures by using the
;;; (:TYPE VECTOR) :NAMED in the DEFSTRUCT definition.  However, TYPEP will *NOT* be able to
;;; recognize the type of one of these structures as anything other than ARRAY or VECTOR.  To check
;;; its type, you must use the predicate function defined by the DEFSTRUCT.  Also, when using any
;;; :TYPE declaration on the DEFSTRUCT, you can no longer specify a :PRINT-FUNCTION value.


(DEFUN SCL:DESCRIBE-FUNCTION (fspec &key (stream *standard-output*))
   "dummy version of Symbolics function that just calls DESCRIBE"
   (let ((*standard-output* stream))
     (describe fspec t)))


(DEFUN DBG:DESCRIBE-GLOBAL-HANDLERS ()
   "dummy version of Symbolics function"
   (print "There are no global handlers")
   nil)


(DEFUN TV:%DRAW-CHAR-CLIPPED-INTERNAL (index font x y alu raster bounds)
  (w:with-clipping-rectangle ((first bounds) (second bounds) (third bounds) (fourth bounds))
    (sys:%draw-char font index x y alu raster)))

(DEFUN TV:%DRAW-LINE-INTERNAL (x1 y1 x2 y2 alu draw-end-point raster)
  (sys:%draw-line x1 y1 x2 y2 alu draw-end-point raster))

(DEFUN GRAPHICS:DRAW-STRING (string start-x start-y &key toward-x toward-y
			     (stream *standard-output*) (alu :draw) (stretch-p nil) (mask nil))
  "Simplified version of Symbolics function (toward-x, toward-y, stretch-p, and mask ignored)."
  (declare (ignore toward-x toward-y stretch-p mask))
  ;; May want to use the :draw-string method instead??? 
  (sys:%draw-string (ticl:send stream :default-font) string 0 nil start-x start-y
		    (case alu
		      (:draw w:alu-seta)
		      (:erase w:alu-andca)
		      (:flip w:alu-xor))
		    stream))

(DEFUN TV:%DRAW-STRING-CLIPPED-INTERNAL (raster alu x y string font index limit bounds)
  (w:with-clipping-rectangle ((first bounds) (second bounds) (third bounds) (fourth bounds))
    (sys:%draw-string font string index limit x y alu raster)))


;;; - F - F - F - F - F - F - F - F - F - F -
(FORMAT:DEFFORMAT FORMAT: (:ONE-ARG) (arg parameters)
   "similar to ~C except everything is printed in a logzenge"
   (if (ticl:send *standard-output* :operation-handled-p :display-lozenged-string)
       (ticl:send *standard-output* :display-lozenged-string (format () "~:C" arg))
       (format:format-ctl-character arg parameters)))

(proclaim '(special original-font-map))
(DEFVAR FORMAT:-FONT-STACK ()	   ; used only by ~ and ~ below
   "a LIFO stack of dotted pairs, (current-font . first-in-font-map), recording the state of *STANDARD-OUTPUT*
at the time ~ was encountered")

(FORMAT:DEFFORMAT FORMAT: (:NO-ARG) (parameters)
   (block format:
     (when (not (ticl:send *standard-output* :operation-handled-p :font-map))
         ;; then this output stream cannot handle font changes, so push NIL onto -FONT-STACK and return
       (push nil format:-font-stack)
       (return-from format:))

     ;; arm the unwind-protect
     (when (null original-font-map)
       (setf original-font-map (ticl:send *standard-output* :font-map)))
     
     ;; normalize parameters into a character style spec
     (setf parameters (first parameters))
     (setf parameters
	   (typecase parameters
	     (scl:character-style parameters)
	     (character (case parameters
			  ((#\b #\B) (scl:make-character-style nil :bold))
			  ((#\i #\I) (scl:make-character-style nil :italic))
			  ((#\p #\P) (scl:make-character-style nil :bold-italic))
			  ((#\r #\R) (scl:make-character-style nil :roman))))
	     (ticl:font (si:backtranslate-font parameters))
	     (cons      (si:parse-character-style parameters))
	     (symbol    (si:backtranslate-font (symbol-value parameters)))))
     (let* ((font-map         (ticl:send *standard-output* :font-map))
	    (current-font     (ticl:send *standard-output* :current-font))
	    (new-font-name    (si:get-font nil nil
					   (si:merge-character-styles parameters
								      (si:backtranslate-font current-font))))
	    (new-current-font (symbol-value new-font-name)))
       (push (cons current-font (aref font-map 0)) format:-font-stack)
       (setf (aref font-map 0) new-current-font)
	 
       (ticl:send *standard-output* :set-font-map     font-map) 
       (ticl:send *standard-output* :set-current-font new-current-font)))
   );;format-


(FORMAT:DEFFORMAT FORMAT:\ (:NO-ARG) (ignore)
   (let ((font-pair (pop format:-font-stack)) ;unconditionally pop the stack
	 (font-map  (ticl:send *standard-output* :send-if-handles :font-map)))
     (when (and (consp font-pair) (vectorp font-map))
       ;; then we have a font and the output stream can take fonts, so establish it as the current font if possible
       (setf (aref font-map 0) (rest font-pair))
       (ticl:send *standard-output* :set-font-map     font-map)
       (ticl:send *standard-output* :set-current-font (first font-pair)))))
       

(TICL:ADVISE FORMAT:FORMAT-CTL-STRING :AROUND FONT-UNWIND-PROTECT nil
   (if (ticl:send *standard-output* :operation-handled-p :font-map)
       ;; then we potentially can have a fonted format, so prepare for it
       (let ((format:-font-stack format:-font-stack)
	     (original-font-map   (copy-seq (ticl:send *standard-output* :font-map)))
	     (current-font        (ticl:send *standard-output* :current-font)))
	 (unwind-protect
	     (progn :do-it)
	   (when (and (vectorp original-font-map)
		      (consp format:-font-stack))
	     ;; then we apparently have terminated abnormally
	     (ticl:send *standard-output* :set-font-map original-font-map)
	     (ticl:send *standard-output* :set-current-font current-font))))

       ;; else we cannot have a fonted format, so never mind
       :do-it))
 
;;; - G - G - G - G - G - G - G - G - G - G -
(DEFUN SYS:GET-FONT (device character-set style &optional (error-p t) inquiry-only)
   "simplified version of Symbolics function (DEVICE, CHARACTER-SET,  ERROR-P, and INQUIRY-ONLY are ignored)"
   (declare (values font-symbol)
	    (ignore device character-set error-p inquiry-only))
     (let ((font (second (assoc style *character-style-explorer-font-alist* :test #'character-style-eql))))
       (if (consp font)
	   (sys:get-font :ignore :ignore font)
	   font)))


;;; - I - I - I - I - I - I - I - I - I - I -


;;; - L - L - L - L - L - L - L - L - L - L -
(DEFUN SYS:LOCALIZE-LIST (list &optional area)
   "dummy version of Symblics function that just returns LIST"
   (declare (ignore area))
   list)

(DEFUN SYS:LOCALIZE-TREE (tree &optional n-levels area)
   "dummy version of Symbolics function that just returns TREE"
   (declare (ignore n-levels area))
   tree)

(proclaim '(inline sys:location-contents))
(DEFUN SYS:LOCATION-CONTENTS (locative)
   (ticl:contents locative))
(defsetf sys:location-contents sys:set-cdr)


;;; - M - M - M - M - M - M - M - M - M - M -
(proclaim '(inline make-character))
(DEFUN SCL:MAKE-CHARACTER (char &key (bits 0) (style nil))
   "Identical to (MAKE-CHAR char bits).  STYLE is ignored."
   (declare (ignore style))
   (make-char char bits))

;;; DW:MEMBER-SEQUENCE
(DEFUN (:PROPERTY DW:MEMBER-SEQUENCE SYS:TYPE-PREDICATE) (object sequence)
  (and (stringp object)
       (member object sequence :test 'string-equal)))

(DEFUN (:PROPERTY DW:MEMBER-SEQUENCE SYS:TYPE-VALIDATOR) (sequence)
  (typep sequence 'sequence))

(DEFUN SI:MERGE-CHARACTER-STYLES (style default-style)
   (multiple-value-bind (character-style family face size color)
       (si:parse-character-style style)
     (when (null default-style) (return-from si:merge-character-styles character-style))
     (multiple-value-bind (default-character-style default-family default-face default-size default-color)
	 (si:parse-character-style default-style)
       (declare (ignore default-character-style))
       (let ((new-family (or family default-family))
	     (new-face   (or face   default-face))
	     (new-size   (or size   default-size))
	     (new-color  (or color  default-color)))
	 (when (member new-size '(:smaller :bigger :larger))
	    ;; then we have a relative size to worry about
	   (setf new-size (if (eq new-size :smaller)
			      (or (first (rassoc default-size *smaller-larger-size-pairs*))
				  new-size)
			      (or (rest (assoc default-size *smaller-larger-size-pairs*))
				  new-size))))
	 (if (and (eq family new-family)
		  (eq face   new-face)
		  (eq size   new-size)
		  (eq color  new-color))
	       ;; then nothing has changed, so return original STYLE
	     style
	     
	       ;; else return new style
	     (scl:make-character-style new-family new-face new-size new-color))))))

(DEFVAR TV:*MOUSE-MODIFYING-KEYSTATES* '(:control :meta :super :hyper))

;;; - N - N - N - N - N - N - N - N - N - N -
(DEFUN SCL:NSTRING-FLIPCASE (string &key (start 0) end)
   "Destructively converts upper case characters in STRING into lower case and visa-versa."
   (setf string (string  string))
   (do ((i          start (1+ i)))
       ((>= i (or end (length string)))
	string)
     (setf (aref string i) (if (upper-case-p (aref string i))
			       (char-downcase (aref string i))
			       (char-upcase   (aref string i))))))



;;; - P - P - P - P - P - P - P - P - P - P -
(DEFUN SI:PARSE-CHARACTER-STYLE (style-spec)
   (if (character-style-p style-spec)
       ;; then we were given a pre-parsed style, so just return it
       (values style-spec (cs-family style-spec)
	       (cs-face   style-spec)
	       (cs-size   style-spec)
	       (cs-color  style-spec))
       
       ;; else we have to parse it
       (if (and (consp style-spec)
		(< (length style-spec) 5)
		(member (first  style-spec) si:*valid-families*)
		(member (second style-spec) si:*valid-faces*)
		(member (third  style-spec) si:*valid-sizes*)
		(typep (fourth style-spec) '(or null (unsigned-byte 8))))
	   ;; then this is a valid style spec, so parse it
	   (values (scl:make-character-style (first  style-spec)
					     (second style-spec)
					     (third  style-spec)
					     (fourth style-spec))
		   (first  style-spec)
		   (second style-spec)
		   (third  style-spec)
		   (fourth style-spec))
	   ;; else this spec is invalid, so report it
	   (ticl:ferror nil "~S is not a valid character style spec" style-spec))))


(DEFUN SYS:PROCESS-FLUSH (process)
   (ticl:send process :flush))


(DEFUN SYS:PROCESS-INTERRUPT (process function &rest args)
   (ticl:lexpr-send process function args))


(DEFUN SYS:PROCESS-KILL (process)
   (ticl:send process :kill))


(DEFMACRO LISP:PUSH (ITEM REFERENCE &KEY AREA LOCALIZE)
   "Same as Common Lisp PUSH except that it accepts--and ignores--AREA and LOCALIZE"
   (declare (ignore area localize))
   (if (symbolp (sys:parse-the-in-place reference))
      ;; special case this to speed up the expansion process and make better code.
      `(setq ,(sys:parse-the-in-place reference) (cons ,item ,reference))
    (multiple-value-bind (tempvars tempargs storevars storeform refform)
	(get-setf-method reference)
      (let ((val (gensym)))
	(sys:sublis-eval-once (cons `(,val . ,item) (pairlis tempvars tempargs))
			  (sys:sublis-eval-once (list (cons (car storevars)
					      `(cons ,val ,refform)))
				  storeform)
			  t t)))))


(DEFMACRO LISP:PUSHNEW (item reference &key (test #'eql) test-not key area localize)
   "Same as Common Lisp PUSHNEW except that it accepts--and ignores--AREA and LOCALIZE"
   (declare (arglist item reference &key test test-not key area localize)
	    (ignore area localize))
   (let ((pl (gensym))
	 (val (gensym)))
     (sys:sublis-eval-once `((,val . ,item))
		       (sys:sublis-eval-once `((,pl . ,reference))
					 `(if (member ,val ,pl :test ,test :test-not ,test-not :key ,key)
					      ,pl
					      (values (setf ,reference (cons ,val ,pl))))))))



;;; - R - R - R - R - R - R - R - R - R - R -


;;; - S - S - S - S - S - S - S - S - S - S -
(DEFMACRO STACK-LET (bindings &body body)
   "Expands STACK-LET as an ordinary LET and trusts TGC to get rid of the garbage"
   `(let ,bindings ,@body))

(DEFUN SCL:STRING-COMPARE (string1 string2 &key (start1 0) (start2 0) end1 end2)
  "similar to ZLC:STRING-COMPARE except that it used keyword arguments"
   (zlc:string-compare string1 string2 start1 start2 end1 end2))


(DEFUN SCL:STRING-EXACT-COMPARE (string1 string2 &key (start1 0) (start2 0) end1 end2)
  "similar to SYS:STRING-COMPARE-CASE except that it used keyword arguments"
   (sys:string-compare-case string1 string2 start1 start2 end1 end2))


(proclaim '(inline scl:string-fat-p))
(DEFUN SCL:STRING-FAT-P (string)
   "returns true if STRING contains elements of type CHARACTER rather than STRING-CHAR"
   (member (array-element-type string) '(t sys::fat-char)))


(DEFUN SCL:STRING-FLIPCASE (string &key (start 0) end)
   "Non-destructively converts upper case characters in STRING into lower case and visa-versa."
   (scl:nstring-flipcase (make-array (length string) :element-type :string-char :initial-contents string)
			  start end))

(proclaim '(inline scl:string-search))
(DEFUN SCL:STRING-SEARCH (key string &key from-end (start1 0) end1 (start2 0) end2)
   "similar to ZLC:STRING-SEARCH & ZLC:STRING-REVERSE-SEARCH with CONSIDER-CASE NIL except it uses keywords 
and has a flag to choose direction"
   (if from-end
       (zlc:string-reverse-search key string start1 end1 start2 end2 nil)
       (zlc:string-search         key string start1 end1 start2 end2 nil)))


(proclaim '(inline scl:string-search-exact))
(DEFUN SCL:STRING-SEARCH-EXACT (key string &key from-end (start1 0) end1 (start2 0) end2)
   "similar to ZLC:STRING-SEARCH or ZLC:STRING-REVERSE-SEARCH with CONSIDER-CASE T 
except it uses keywords and has a flag to choose direction"
   (if from-end
       (zlc:string-reverse-search key string start1 end1 start2 end2 t)
       (zlc:string-search         key string start1 end1 start2 end2 t)))


(proclaim '(inline scl:string-search-char))
(DEFUN SCL:STRING-SEARCH-CHAR (char string &key from-end (start 0) end)
  "similar to ZLC:STRING-SEARCH-CHAR or ZLC:STRING-REVERSE-SEARCH-CHAR with CONSIDER-CASE NIL 
except it uses keywords and has a flag to choose direction"
  (if from-end
      (zlc:string-reverse-search-char char string start end nil)
      (zlc:string-search-char         char string start end nil)))


(proclaim '(inline scl:string-search-char-exact))
(DEFUN SCL:STRING-SEARCH-EXACT-CHAR (char string &key from-end (start 0) end)
  "similar to ZLC:STRING-SEARCH-CHAR or ZLC:STRING-REVERSE-SEARCH-CHAR with CONSIDER-CASE T
except it uses keywords and has a flag to choose direction"
  (if from-end
      (zlc:string-reverse-search-char char string start end t)
      (zlc:string-search-char         char string start end t)))

(DEFUN ZL:STRING-SEARCH-EXACT-CHAR (char string &optional (from 0) to)
  "similar to ZLC:STRING-SEARCH-CHAR with CONSIDER-CASE T"
   (zlc:string-search-char char string from to t))
(export 'zl:STRING-SEARCH-EXACT-CHAR 'zl)

(proclaim '(inline scl:string-search-not-char))
(DEFUN SCL:STRING-SEARCH-NOT-CHAR (char string &key from-end (start 0) end)
  "similar to ZLC:STRING-SEARCH-NOT-CHAR or ZLC:STRING-REVERSE-SEARCH-NOT-CHAR with CONSIDER-CASE NIL 
except it uses keywords and has a flag to choose direction"
  (if from-end
      (zlc:string-reverse-search-not-char char string start end nil)
      (zlc:string-search-not-char         char string start end nil)))


(proclaim '(inline scl:string-search-not-exact-char))
(DEFUN SCL:STRING-SEARCH-NOT-EXACT-CHAR (char string &key from-end (start 0) end)
  "similar to ZLC:STRING-SEARCH-NOT-CHAR or ZLC:STRING-REVERSE-SEARCH-NOT-CHAR with CONSIDER-CASE T 
except it uses keywords and has a flag to choose direction"
  (if from-end
      (zlc:string-reverse-search-not-char char string start end t)
      (zlc:string-search-not-char         char string start end t)))


(proclaim '(inline scl:string-search-set))
(DEFUN SCL:STRING-SEARCH-SET (char-set string &key from-end (start 0) end)
  "similar to ZLC:STRING-SEARCH-SET & ZLC:STRING-REVERSE-SEARCH-SET with CONSIDER-CASE NIL
except it uses keywords and has a flag to choose direction"
   (if from-end
       (zlc:string-reverse-search-set char-set string start end nil)
       (zlc:string-search-set         char-set string start end nil)))


(proclaim '(inline scl:string-search-not-set))
(DEFUN SCL:STRING-SEARCH-NOT-SET (char-set string &key from-end (start 0) end)
  "similar to ZLC:STRING-SEARCH-NOT-SET & ZLC:STRING-REVERSE-SEARCH-NOT-SET with CONSIDER-CASE NIL
except it uses keywords and has a flag to choose direction"
   (if from-end
       (zlc:string-reverse-search-not-set char-set string start end nil)
       (zlc:string-search-not-set         char-set string start end nil)))


;;; - U - U - U - U - U - U - U - U - U - U -
(DEFMACRO SCL:USING-RESOURCE ((&rest resource-spec-list) &environment environment &body body)
  "Execute BODY with VAR bound to an object allocated from resource RESOURCE-NAME.
PARAMETERS are used in selecting or creating the object,
according to the definition of the resource.  You can pass a list of (var resource-name . params)
to create multiple var/resource bindings for the body."
  (declare (arglist {(VAR RESOURCE-NAME . PARAMETERS) \| ({(VAR RESOURCE-NAME . PARAMETERS)}+)} &environment environment &body body))
  (multiple-value-bind (body declarations)
      (parse-body body environment nil)
    (when (atom (first resource-spec-list))
      (setf resource-spec-list (list resource-spec-list))) ;normalize parameters
    `(let (,@(mapcar #'(lambda (resource-spec)
			(list (first resource-spec) nil))
		    resource-spec-list))
       ,@declarations
       (unwind-protect
	   (progn
	     ,@(mapcar #'(lambda (resource-spec)
			`(setq ,(first resource-spec) (ticl:allocate-resource ',(second resource-spec) .
									       ,(cddr resource-spec))))
		       resource-spec-list)
	     . ,body)
	 ,@(mapcar #'(lambda (resource-spec)
			`(and ,(first resource-spec) (ticl:deallocate-resource ',(second resource-spec)
									        ,(first resource-spec))))
		       resource-spec-list)))))

;;; - V - V - V - V - V - V - V - V - V - V -


;;; - W - W - W - W - W - W - W - W - W - W -
(DEFMACRO SCL:WITH-CHARACTER-STYLE ((style &optional (stream '*standard-output*)
					      &key bind-line-height) &body body)
   "simplified version of Symbolics macro (BIND-LINE-HEIGHT is ignored)"
   (declare (ignore bind-line-height))
   (let ((old-current-font (gensym))
	 (old-font-map     (gensym))
	 (new-font         (gensym))
	 (body-name        (gensym)))
     `(flet ((,body-name ()
	      ,@body))			   ; make sure BODY is expanded only once
	(if (ticl:send ,stream :operation-handled-p :font-map)
	    ;; then we can try remapping fonts
	    (let* ((,old-current-font (ticl:send ,stream :current-font))
		   (*current-character-style*
		     (si:merge-character-styles ,style (or (si:backtranslate-font ,old-current-font)
							   *current-character-style*)))
		   (,old-font-map     (ticl:send ,stream :font-map))
		   (,new-font         (or (si:get-font :ignore :ignore *current-character-style*)
					  ,old-current-font)))
	      (unwind-protect
		  (progn
		    (ticl:send ,stream :set-font-map (list ,new-font))
		    (ticl:send ,stream :set-current-font ,new-font)
		    (,body-name)
		  );;progn
	      (ticl:send ,stream :set-font-map ,old-font-map)
	      (ticl:send ,stream :set-current-font ,old-current-font)))
	  ;; else this stream doesn't support fonts, so ignore this macro
	  (,body-name)))))

				      
(DEFMACRO SCL:WITH-CHARACTER-FAMILY ((family &optional (stream *standard-output*)
						 &key bind-line-height) &body body)
   "simplified version of Symbolics macro (BIND-LINE-HEIGHT is ignored)"
   (declare (ignore bind-line-height))
   `(scl:with-character-style ((si:merge-character-styles (list ,family nil nil) *current-character-style)
			      ,stream) ,@body))

				      
(DEFMACRO SCL:WITH-CHARACTER-FACE ((face &optional (stream *standard-output*)
						 &key bind-line-height) &body body)
   "simplified version of Symbolics macro (BIND-LINE-HEIGHT is ignored)"
   (declare (ignore bind-line-height))
   `(scl:with-character-style ((si:merge-character-styles (list nil ,face nil) *current-character-style)
			      ,stream) ,@body))

				      
(DEFMACRO SCL:WITH-CHARACTER-SIZE ((size &optional (stream *standard-output*)
						 &key bind-line-height) &body body)
   "simplified version of Symbolics macro (BIND-LINE-