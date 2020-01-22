

(in-package :zwei)


;;; 137


(defun must-redisplay (window degree &optional line index no-need-for-other-windows)
  "Tell WINDOW that it must do redisplay of degree DEGREE.
DEGREE is a number which you obtain as the value of one of these symbols:
 DIS-NONE  no redisplay needed.
 DIS-MARK-GOES  no redisplay needed except maybe region went away.
 DIS-BPS   point and mark may have moved.
 DIS-LINE  one line has changed.  LINE is the line,
		and INDEX is the index after which changes occurred.
 DIS-TEXT  any text may have changed.
 DIS-ALL   fonts, size, etc. may have changed.
What you specify here is max'd into some variables in the window
 and remembered until the next opportunity for redisplay.
NO-NEED-FOR-OTHER-WINDOWS says that there is no need to redisplay
 other windows displaying the same interval as WINDOW.
This function applies to all kinds of displayers, including non-sheet."
  (let ((w-degree (window-redisplay-degree window)))
    (cond ((= degree dis-line)
	   (cond ((and (window-overprinting-flag window)
		       (position #\backspace (the string (string line))
				 :test #'char-equal))
		  (setf (window-redisplay-degree window) (max w-degree dis-text)))
		 ((= w-degree dis-line)
		  (cond ((eq (window-redisplay-line window) line)
			 (setf (window-redisplay-index window)
			       (min index (window-redisplay-index window))))
			(t
			 (setf (window-redisplay-degree window) dis-text))))
		 ((< w-degree dis-line)
		  (setf (window-redisplay-degree window) dis-line)
		  (setf (window-redisplay-line window) line)
		  (setf (window-redisplay-index window) index))))
	  (t
	   (setf (window-redisplay-degree window) (max w-degree degree)))))
  (or no-need-for-other-windows
      (must-redisplay-other-windows (window-interval window) window degree line index)))


;;; 197

;;; The elements of a window PLINE are the:
;;; PLINE-LINE			;Editor line displayed, NIL if blank
;;; PLINE-FROM-INDEX		;First character displayed
;;; PLINE-TO-INDEX		;Last character displayed+1
;;; PLINE-TICK			;TICK as of last time pline updated on display
;;; PLINE-MARKING-LEFT		;NIL no marking, or X coord of start of region-marking
;;; PLINE-MARKING-WIDTH		;Horizontal extent of marking
;;; PLINE-TEXT-WIDTH		;Horizontal extent of text
;;; Note that for non-continuation lines, PLINE-TEXT-WIDTH includes a little
;;; extra for the pseudo-space at the end of the line which corresponds to the #\CR.
;;; But for continuation lines, it does not include the ! at the end of the line.
;;; (It does now, but that should be regarded as a bug in SHEET-LINE-OUT)
;;; PLINE-TEXT-WIDTH is used only for region marking.

(DEFUN REDISPLAY (WINDOW &OPTIONAL (RECENTER-TYPE :POINT) RC1 RC2 (FORCE-TO-COMPLETION-P NIL))
  "Redisplay WINDOW (a displayer).
RECENTER-TYPE says how to recompute where to display from.
 :ABSOLUTE means that RC1 is a fraction saying where
  point should go in the window (or NIL, meaning use *CENTERING-FRACTION*);
 :RELATIVE means that point should be RC1 plines below where it is now;
 :START says that RC1 is a BP to start displaying at,
  or else RC1 is a line and RC2 is the index within it.
 :NONE means do not recenter the window even if point is off screen.
 :POINT means recenter only if point is off screen,
  and if so, put point RC1 fraction of the way down the screen.
  (If RC1 is NIL, use *CENTERING-FRACTION*)
Redisplay is done according to the degree that WINDOW remembers it needs,
and stops when input is found to be available."
  (when (getf (node-property-list (window-interval window)) :killed)
    ;; may 02/17/89  This corrects last several patches to this function.
    ;; We must 1. get a non :killed buffer. We used to do :
    ;; (make-buffer-current (CAR *ZMACS-BUFFER-LIST*)) which almost worked
    ;; except the (zmacs-buffer :select) method was putting the :killed
    ;; buffer back on the list sometimes. The modified :Select method
    ;; needs *window* bound or sometimes (window :redisplay) will croak on
    ;; point's that are in the killed buffer/interval when processing the
    ;; SPECIAL-BLINKER-LIST - make-buffer-current only works for *window*
    ;; but redisplay-all-windows calls (redisplay window) on (WINDOW-LIST).
    ;; :FIX-WINDOW-INTERVAL is correct (since :select method fixed) because
    ;; it also updates *window*, if necessary, and takes care of misc items.
    (send (window-interval window) :fix-window-interval window)) ;; may 02/17/89
  (send window :redisplay recenter-type rc1 rc2 force-to-completion-p))
