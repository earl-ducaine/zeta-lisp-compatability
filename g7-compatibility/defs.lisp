;; -*- Mode:Common-Lisp; Package:(G7C LISP); Fonts:(MEDFNT HL12B HL12I MEDFNT MEDFNTB); Patch-file:T; Base:10 -*-

;;; "Genera" and "Symbolics" are trademarks of Symbolics, Inc.
;;; "Explorer" is a trademark of Texas Instruments.

;;; - A - A - A - A - A - A - A - A - A - A -
(DEFVAR SYS:*ALL-GENERIC-FUNCTION-NAMES* ())

(DEFCONSTANT  SCL:ARRAY-LEADER-LENGTH-LIMIT 1024 "upper exclusive limit of an array leader")

(DEFVAR SYS:*ARRAY-TYPE-CODES* ticl:array-types)  
(ticl:forward-value-cell 'sys:*array-type-codes*  'ticl:array-types)


;;; - B - B - B - B - B - B - B - B - B - B - 
(DEFTYPE SCL:BOOLEAN () `(member t nil))


;;; - C - C - C - C - C - C - C - C - C - C -

SCL:(DEFSTRUCT (CHARACTER-STYLE (:print-function g7c:print-character-style) (:conc-name "CS-")
			      (:constructor make-character-style (&optional family face size color)))
   (family nil)
   (face   nil)
   (size   nil)
   (color  nil))

(defun print-character-style (object stream print-level)
   (if (and (numberp *print-level*) (>= print-level *print-level*))
       (princ "#" stream)
       (let ((string-1 (format nil "~A.~A.~A" (cs-family object) (cs-face object) (cs-size object)))
	     (string-2 (when (integerp (cs-color object)) (format nil ".~D" (cs-color object)))))
	 (if *print-escape*
	     (sys:printing-random-object (object stream :typep)
	       (princ string-1 stream )
	       (when (not (null string-2)) (princ string-2 stream)))
	     (progn (princ string-1 stream)
		    (when (not (null string-2)) (princ string-2 stream)))))))


(DEFVAR *CHARACTER-STYLE-EXPLORER-FONT-ALIST*
	 '(((:fix   :roman          :tiny)       fonts:tiny)
	   ((:fix   :uppercase      :very-small) fonts:5x5)
	   ((:fix   :roman          :small)      fonts:tvfont)
	   ((:fix   :roman          :normal)     fonts:cptfont)
	   ((:fix   :roman          :normal)     fonts:mouse)
	   ((:fix   :bold           :normal)     fonts:cptfontb)
	   ((:fix   :bold-extended  :normal)     fonts:cptfontb)
	   ((:fix   :bold-italic    :normal)     fonts:cptfontbi)
	   ((:fix   :italic         :normal)     fonts:cptfonti)
	   ((:fix   :roman          :large)      fonts:medfnt)
	   ((:fix   :roman          :large)      fonts:courier)
	   ((:fix   :bold           :large)      fonts:medfntb)
	   ((:fix   :extended       :large)      fonts:wider-medfnt)
	   ((:fix   :extra-extended :large)      fonts:wider-font)
	   ((:fix   :roman          :very-large) fonts:bigfnt)
	   ((:dutch :roman          :very-small) fonts:tr8)
	   ((:dutch :bold           :very-small) fonts:tr8b)
	   ((:dutch :italic         :very-small) fonts:tr8i)
	   ((:dutch :roman          :small)      fonts:tr10)
	   ((:dutch :bold           :small)      fonts:tr10b)
	   ((:dutch :italic         :small)      fonts:tr10i)
	   ((:dutch :bold-italic    :small)      fonts:tr10bi)
	   ((:dutch :roman          :normal)     fonts:tr12)
	   ((:dutch :bold           :normal)     fonts:tr12b)
	   ((:dutch :italic         :normal)     fonts:tr12i)
	   ((:dutch :bold-italic    :normal)     fonts:tr12bi)
	   ((:dutch :roman          :very-large) fonts:mets)
	   ((:dutch :bold           :very-large) fonts:metsb)
	   ((:dutch :italics        :very-large) fonts:metsi)
	   ((:dutch :bold-italics   :very-large) fonts:metsbi)
	   ((:swiss :roman          :tiny)       fonts:hl6)
	   ((:swiss :roman          :very-small) fonts:hl7)
	   ((:swiss :roman          :small)      fonts:hl10)
	   ((:swiss :bold           :small)      fonts:hl10b)
	   ((:swiss :roman          :normal)     fonts:hl12)
	   ((:swiss :bold           :normal)     fonts:hl12b)
	   ((:swiss :italics        :normal)     fonts:hl12i)
	   ((:swiss :bold-italics   :normal)     fonts:hl12bi)
	   ((:device-font :tr18     :normal)     fonts:tr18)
	   ((:device-font :tr18b    :normal)     fonts:tr18b)
	   ((:device-font :43vxms   :normal)     fonts:43vxms)
	   ((:device-font :instruments :normal)  fonts:instruments)
	   ((:device-font :icons    :normal)     fonts:icons))
   "an alist associating a Genera 7 character style, (:family :face :size), with an Explorer font symbol or another style")

(DEFVAR *CURRENT-CHARACTER-STYLE* '(:fix :roman :normal nil))
(DEFVAR SCL:*CURRENT-PROCESS* ticl:current-process)  
(ticl:forward-value-cell 'scl:*current-process*   'ticl:current-process)


;;; - D - D - D - D - D - D - D - D - D - D -
(DEFVAR SCL:*DEFAULT-CONS-AREA* ticl:default-cons-area) 
(ticl:forward-value-cell 'scl:*default-cons-area* 'ticl:default-cons-area)

(DEFVAR SI:*DEFAULT-PROCESS-ABOUT-TIMEOUT* 300 "dummy version of Symbolics variable")
(DEFVAR TV:*DIM-SCREEN-AFTER-N-MINUTES-IDLE* nil "dummy version of Symbolics variable")
(DEFVAR TV:DISPLAY-DEVICE-TYPE nil "This variable is actually an instance variable for windows on the Symbolics")


;;; - F - F - F - F - F - F - F - F - F - F - 
(DEFVAR TV:*FUNCTION-KEYS* w:*terminal-keys*)
(ticl:forward-value-cell 'tv:*function-keys* 'w:*terminal-keys*)


;;; - M - M - M - M - M - M - M - M - M - M - 
(DEFCONSTANT W:MOUSE-GLYPH-THICK-LEFT-RIGHT-ARROW 14)


;;; - P - P - P - P - P - P - P - P - P - P - 
(DEFSETF TICL:PLANE-AREF (plane x y) (datum)
   ;; special case for two subscripts
   `(progn (apply #'ticl:plane-aset ,datum ,plane ,x ,y)
	   ,datum))

(DEFVAR SCL:*PRINT-ARRAY-LENGTH* nil "dummy version of Symbolics variable")
(DEFVAR SCL:*PRINT-BIT-VECTOR-LENGTH* nil "dummy version of Symbolics variable")
(DEFVAR SCL:*PRINT-STRING-LENGTH* nil "dummy version of Symbolics variable")
(DEFVAR SI:*PROCESSES-FORCIBLY-ABORTED* nil "dummy version of Symbolics variable")


;;; - R - R - R - R - R - R - R - R - R - R -
(DEFVAR COMPILER:*RETURN-STYLE-CHECKER-ON* nil "dummy version of Symbolics variable")


;;; - S - S - S - S - S - S - S - S - S - S -
(DEFVAR TV:*SCREEN-DIMNESS-PERCENT 100 "dummy version of Symbolics variable")

(DEFVAR TV:*SELECT-KEYS* w:*system-keys*)
(ticl:forward-value-cell 'tv:*select-keys* 'w:*system-keys*)

(DEFVAR SI:*STANDARD-CHARACTER-SET* nil
   "This is a variable on the Symbolics, we just need it to be defined")



(DEFTYPE LISP:STRUCTURE ()
   "object is probably a Common Lisp structure"
   '(satisfies g7c:structure-p))
(export 'lisp:structure 'lisp)
(defun structure-p (object)		   ; used only by DEFTYPE above
   ;;Returns true if OBJECT is likely to have been defined by a DEFSTRUCT.
  ;; Used only by DEFTYPE above
   (let ((structure-name (or (ticl:named-structure-p object)  ; case of default DEFSTRUCT (most common)
			     (and (typep object 'sequence)    ; case of (:TYPE VECTOR) or (:TYPE LIST) :NAMED
				  (> (length object) 1)
				  (elt object 0)))))
     (when (and (symbolp structure-name) (get structure-name 'sys:defstruct-description))
       structure-name)))


;;; - V - V - V - V - V - V - V - V - V - V -
(DEFVAR SI:*VALID-FACES*    '(nil :bold :bold-condensed-caps :bold-extended :bold-italic
				 :condensed :condensed-caps :extra-condensed :expended
				 :extra-extended :italic :math :roman :symbol :uppercase))
(DEFVAR SI:*VALID-FAMILIES* '(nil :device-font :dutch :fix :normal-body :sans-serif-body
				 :serif-body :swiss))
(DEFVAR SI:*VALID-SIZES*    '(nil :smaller :same :bigger :larger :tiny :very-small :small
				 :normal :large :very-large :huge))

(DEFVAR *SMALLER-LARGER-SIZE-PAIRS*
	 '((:tiny . :very-small) (:tiny . :tiny) (:very-small . :small) (:small . :normal)
	   (:normal . :large) (:large . :very-large) (:very-large . :huge) (:huge . :huge))
   "association list of character style sizes such that (REST (ASSOC size ...)) => next larger size
while (FIRST (RASSOC size ...)) => next smaller size")

;;; This defines all of the Symbolics mouse characters to the system.
;;; This should also be done for the Symbolics arrow character set, but most of
;;; the arrow characters do not exist on the Explorer and would need to be created.
(setf sys:xr-special-character-names
      (nconc sys:xr-special-character-names
	     `((mouse:up-arrow                     . ,w:mouse-glyph-thin-up-arrow)
	       (mouse:right-arrow                  . ,w:mouse-glyph-thin-right-arrow)
	       (mouse:down-arrow                   . ,w:mouse-glyph-thin-down-arrow)
	       (mouse:left-arrow                   . ,w:mouse-glyph-thin-left-arrow)
	       (mouse:vertical-double-arrow        . ,w:mouse-glyph-thin-up-down-arrow)
	       (mouse:horizontal-double-arrow      . ,w:mouse-glyph-thin-left-right-arrow)
	       (mouse:NW-arrow                     . ,w:mouse-glyph-north-west-arrow)
	       (mouse:times                        . ,w:mouse-glyph-thin-cross)
	       (mouse:fat-up-arrow                 . ,w:mouse-glyph-thick-up-arrow)
	       (mouse:fat-right-arrow              . ,w:mouse-glyph-thick-right-arrow)
	       (mouse:fat-down-arrow               . ,w:mouse-glyph-thick-down-arrow)
	       (mouse:fat-left-arrow               . ,w:mouse-glyph-thick-left-arrow)
	       (mouse:fat-double-vertical-arrow    . ,w:mouse-glyph-thick-up-down-arrow)
	       (mouse:fat-double-horizontal-arrow  . ,w:mouse-glyph-thick-left-right-arrow)
	       (mouse:paragraph                    . ,w:mouse-glyph-paragraph)
	       (mouse:NW-corner                    . ,w:mouse-glyph-upper-left-corner)
	       (mouse:SE-corner                    . ,w:mouse-glyph-lower-right-corner)
	       (mouse:hourglass                    . ,w:mouse-glyph-hourglass)
	       (mouse:circle-plus                  . ,w:mouse-glyph-circle-plus)
	       (mouse:paintbrush                   . ,w:mouse-glyph-paint-brush)
	       (mouse:scissors                     . ,w:mouse-glyph-scissor)
	       (mouse:trident                      . ,w:mouse-glyph-trident)
	       (mouse:NE-arrow                     . ,w:mouse-glyph-north-east-arrow)
	       (mouse:circle-times                 . ,w:mouse-glyph-circle-x)
	       (mouse:big-triangle                 . ,w:mouse-glyph-large-right-triangle-pointer)
	       (mouse:medium-triangle              . ,w:mouse-glyph-medium-right-triangle-pointer)
	       (mouse:small-triangle               . ,w:mouse-glyph-small-right-triangle-pointer)
	       (mouse:inverse-up-arrow             . ,w:mouse-glyph-block-up-arrow)
	       (mouse:inverse-down-arrow           . ,w:mouse-glyph-block-down-arrow)
	       (mouse:filled-lozenge               . ,w:mouse-glyph-medium-diamond)
	       (mouse:dot                          . ,w:mouse-glyph-small-dot)
	       (mouse:fat-times                    . ,w:mouse-glyph-thick-cross)
	       (mouse:small-filled-circle          . ,w:mouse-glyph-small-solid-circle)
	       (mouse:filled-circle                . ,w:mouse-glyph-medium-solid-circle)
	       (mouse:fat-circle                   . ,w:mouse-glyph-hollow-circle)
	       (mouse:fat-circle-minus     