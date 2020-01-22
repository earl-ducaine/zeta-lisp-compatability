
(in-package :zwei)

;; line 488


;; Defines a command.  Form is:
;; (defcom com-foo "Documentation." options-list . body)
;; Note: unlike eine, there is no lambda-list.
(defmacro defcom (fn doc options &body def)
  "Options are:
   (KM)   -- This command always preserves MARK.
   (SM)   -- Set MARK. There will be a region after this command
   (NM)   -- Reset MARK. There will be no region after this command
   (PUSH) -- Push POINT onto the point-pdl
   (R)    -- Frob recentering fraction for forward move
   (-R)   -- Frob recentering fraction for backward move."
  `(eval-when (:compile-toplevel)
     (progn
       (command-define ',fn ',doc ',options)
       (defun ,fn ()
	 ,@(process-command-options options)
	 ,@def))))


;;; line 504


(defvar *command-alist* ()
   "Alist of all ZWEI command names (strings) versus the commands
    themselves (symbols).")

(defun command-define (command doc ignore)
  (cond ((stringp doc)
	 (setf (documentation command 'function) doc))
	((or (symbolp doc)
	     (and (not (atom doc))
		  (member (car doc) '(function lambda) :test #'eq)))
	 (setf (get command 'documentation-function) doc))
	(t
	 (ferror nil "the command ~s has invalid self-documentation ~s" command doc)))
  (let* ((name (make-command-name command))
	 (aentry (assoc name *command-alist* :test 'equalp)))
    (setf (get command 'command-name) name)
    (if aentry
	(unless (eq command (cdr aentry))
	  (when (fquery nil (str "the command name ~s is currently "
				 "set up to call ~s; ~%"
				 "redefine it to call ~s? ")
			name (cdr aentry) command)
	    (setf (cdr aentry) command)))
	(push (cons name command) *command-alist*))))


;;; line 531

(defun process-command-options (options)
  (do ((l options (cdr l))
       (ret nil
	    (append
	     (cdr
	      (assoc (car l)
		     '((nm (setf (window-mark-p *window*) nil))
		       (sm (setf (window-mark-p *window*) t)
			(setq *mark-stays* t))
		       (km (setq *mark-stays* t))
		       (r (setq *centering-fraction*
			   (if (plusp *numeric-arg*)
			       *min-reset-fraction*
			       *max-reset-fraction*)))
		       (-r (setq *centering-fraction*
			    (if (plusp *numeric-arg*)
				*max-reset-fraction*
				*min-reset-fraction*)))
		       (push (point-pdl-push (point) *window*))
		       (otherwise (ferror nil "unknown defcom option ~s" (car l))))
		     :test #'eq))
	     ret)))
      ((null l) ret)))


;;; line 550

;;; Convert a string into human-readable form.  Remove leading COM-, or leading
;;; and trailing *'s.  Conver hyphens into spaces, and capitalize each word.
;;; This is used both for command names and variable names.
(defun make-command-name (command)
  (setq command (string command))
  (let ((clen (length command)))
    (let ((str (subseq command
		       (cond ((string-equal "com-" command :start1 0 :start2 0 :end1 4 :end2 4) 4)
			     ((string-equal "*" command :start1 0 :start2 0 :end1 1 :end2 1) 1)
			     (t 0))
		       (cond ((char-equal #\* (aref command (1- clen))) (1- clen))
			     (t clen)))))
      (string-capitalize-words str))))
