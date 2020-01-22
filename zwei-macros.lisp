

(in-package :zwei)


;;; 224


;;; Redisplay levels. These are symbolic constants. They have global
;;; values and should never be bound.
(defparameter dis-none 0
  "As value from a command, or to must-redisplay, means: no redisplay
   needed.")

(defparameter dis-mark-goes 1
  "As value from a command, or to must-redisplay, means: consider
   flushing region-marking")

(defparameter dis-bps 2
  "As value from a command, or to must-redisplay, means: point and
   mark may be changed.")

(defparameter dis-line 3
  "To must-redisplay, means: changes within one line.
   Should be accompanied by additional arguments to must-redisplay
   giving the line, and the index at which changes start.  If a
   command returns DIS-LINE, it should return a line and index as
   additional values.")

(defparameter dis-text 4
  "As value from a command, or to MUST-REDISPLAY, means: Text may have
  changed.")

(defparameter dis-all 5
  "As value from a command, or to must-redisplay, means: window
   parameters may have changed. The window must be completely
   redisplayed.")


;;; 373


;; Macros used to make command easy to write.
(defmacro point ()
  "The current editing pointer in the current window."
  '(hi:window-point hi::*current-window*))


;;; 466

;; something like the following from Hemlock: searchcoms.lisp is
;; needed

;; (defun replace-that-case (lower cap upper mark length dumb)
;;   (character-offset mark (- length))
;;   (let ((insert (cond (dumb lower)
;;                       ((upper-case-p (next-character mark))
;;                        (mark-after mark)
;;                        (prog1 (if (upper-case-p (next-character mark)) upper cap)
;;                               (mark-before mark)))
;;                       (t lower))))
;;     (with-mark ((undo-mark1 mark :left-inserting)
;;                 (undo-mark2 mark :left-inserting))
;;       (character-offset undo-mark2 length)
;;       (push (make-replace-undo
;;              ;; Save :right-inserting, so the INSERT-STRING at mark below
;;              ;; doesn't move the copied mark the past replacement.
;;              (copy-mark mark :right-inserting)
;;              (delete-and-save-region (region undo-mark1 undo-mark2)))
;;             *query-replace-undo-data*))
;;     (insert-string mark insert)))




(defmacro with-undo-save ((type bp1 bp2 in-order-p) &body body)
  "Record changes made by body to interval from bp1 to bp2 for undo.
   type is a string saying the kind of operation, for the user. It
   should start with a capital letter."
  `(unwind-protect
	(progn
	  ,@body)
     ;; todo ed -- not correct but a reminder to put something correct
     (hemlock::undo-command)))


;;; 476


(defmacro with-undo-save-if (cond-form (type bp1 bp2 in-order-p) &body body)
  "Record changes made by BODY to interval from BP1 to BP2 for undo,
   if cond-form evals non-nil. type is a string saying the kind of
   operation, for the user. It should start with a capital letter."
  (let ((gensym (gensym)))
    `(let ((,gensym ,cond-form))
       (unwind-protect
	   (let-if ,gensym ((*batch-undo-save* t))
	     (when ,gensym (undo-save-begin ,bp1 ,bp2 ,in-order-p ,type))
	     ,@body)
	 (when ,gensym (undo-save-end))))))
