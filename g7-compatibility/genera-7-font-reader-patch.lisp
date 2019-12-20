;;; -*- cold-load:t; Mode:Common-Lisp; Package:SI ; Base:10 -*- file.

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (b)(3)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1985,1987 Texas Instruments Incorporated. All rights reserved.
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;; File created by LSS

;; BACKGROUND:  Symbolics Genera 7 files do not have to have fonts specified in their mode 
;;    line to use fonts.  Once you read their font password, then you are in 
;;    "FAT MODE" and should treat the  character specially.
;;    Once you read their "Unpassword", then you are in "THIN MODE", and
;;    an  is an .
;;    Furthermore, when in FAT MODE, the specification that comes after an
;;     can be an entire Lisp expression, so we need to ignore the whole thing
;;    in the Reader.



(defvar *SYMBOLICS-EPSILON-FONT-PASSWORD* "D,#TD1PsT[Begin using 006 escapes]")

(DEFFLAVOR BUFFERED-INPUT-CHARACTER-STREAM
	   ((symbolics-fat-mode nil)  ;; use symbolics fonts - LSS
	    (symbolics-thin-mode nil)) ;; don't convert epsilons - LSS
   (INPUT-POINTER-REMEMBERING-MIXIN BUFFERED-LINE-INPUT-STREAM)
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  (:DOCUMENTATION :COMBINATION "A buffered input character stream, gives :LINE-IN."))



(defun CHECK-FOR-SYMBOLICS-FONT-PASSWORD-IN-STREAM (stream)
  "We have already read an  and a .  Look for the rest of the password."
  (let ((s *symbolics-epsilon-font-password*))
    (do ((n (length s))
	 (j 0 (1+ j)))
	((= j n) t)  ;; return condition
      (let ((new-ch (send stream :tyi)))
	(unless (char= new-ch (aref s j))
	  (send stream :untyi new-ch)
	  (do((k (1- j) (1- k)))
	     ((= k -1))
	    (send stream :untyi (aref s k)))
	  (return nil))))))



(defun POSSIBLY-DISCARD-EPSILON-DATA (stream)
  ;; The reader has read an  character, possibly signalling the beginning of a font
  ;;  code, which we probably want to ignore.
  (let ((ch (SEND STREAM :TYI)))
    (cond (
	   ;; Explorer fonts = OFF  AND  Symbolics fonts = OFF
	   (and (not READ-DISCARD-FONT-CHANGES)
		(or (send stream :symbolics-thin-mode)
		    (not (send stream :symbolics-fat-mode))))
	   ;; We don't handle fonts, except to look for the beginning of a Symbolics font password
	   (cond
	     ((eq ch (char-int #\))
	      (cond ((check-for-symbolics-font-password-in-stream stream)
		     (send stream :set-symbolics-fat-mode t)
		     (send stream :set-symbolics-thin-mode nil)
		     nil)
		    (t
		     (send stream :untyi ch)
		     #\)))
	     (t
	      (send stream :untyi ch)
	      #\)))
	  (
	   ;; Explorer fonts = ON  AND  Symbolics fonts = OFF
	   (and READ-DISCARD-FONT-CHANGES
		(or (send stream :symbolics-thin-mode)
		    (not (send stream :symbolics-fat-mode))))
	   ;; We look for a Symbolics password and for Explorer fonts
	   (cond ((eq ch (char-int #\))
		  #\)
		 ((eq ch (char-int #\))
		  (when (check-for-symbolics-font-password-in-stream stream)
		    (send stream :set-symbolics-fat-mode t)
		    (send stream :set-symbolics-thin-mode nil)
		    nil))))
	  (
	   ;; Symbolics Fonts = ON  --  Explorer Fonts = OFF or ON
	   (send stream :symbolics-fat-mode)
	   ;; We check for all possibilities
	   (cond ((eq ch (char-int #\ ))
		  #\ )
		 ((eq ch (char-int #\( ))
		  (send stream :untyi ch)
		  (SI:INTERNAL-READ STREAM T nil nil nil t)    ;; read the next lisp expression (font info)
		  nil)
		 ((eq ch (char-int #\))
		  (when (check-for-symbolics-font-password-in-stream stream)
		    (send stream :set-symbolics-fat-mode t)
		    (send stream :set-symbolics-thin-mode nil)
		    nil))
		 ((eq ch (char-int #\))     ;; ** Symbolics UnPassword - don't use fonts anymore
		  (send stream :set-symbolics-fat-mode nil)
		  (send stream :set-symbolics-thin-mode t)
		  nil))))))


(defun INTERNAL-READ-CHAR (stream  &optional (errorp t) eofval noactivation &aux ch)
  (loop
    (SETQ CH (SEND STREAM (IF RUBOUT-HANDLER ':ANY-TYI ':TYI) errorp))
    (COND ((NULL CH)
	   (return eofval))
	  ((atom ch)
	   (setf (char-font ch) 0)
	   (if (or (neq ch (char-int #\))
		   (not (send stream :operation-handled-p :symbolics-fat-mode)))
	       (RETURN (setf LAST-CHAR-READ (int-char ch)))
	       ;;else
	       (let ((new-char (possibly-discard-epsilon-data stream)))
		 (when new-char (return (int-char new-char))))))
	  ((and (CONSP CH) 
		(EQ (CAR CH) ':ACTIVATION)
		;; Ignore activations except in top-level context.
		(null noactivation))
	   (null errorp)       ;;this looks like a goof in the original code - LSS ****
	   (return eofval)))))










