

(in-package :zwei)

;;; 142

(defun create-comtab (&optional name)
  "Return a new, empty comtab with name name."
  (make-comtab comtab-keyboard-array (make-array '(400 20))
	       comtab-name          name
	       comtab-mouse-array (make-array '(2 3 20))))

;;; 186

(defun set-comtab (comtab specs &optional extended-commands)
  "Set the definitions of some characters and extended commands in comtab.
   SPECS is an alternating list of characters (or character names) and
   definitions. A list may also appear instead of a character; this
   means set several successive characters. The car of the list is the
   first character, and the cdr is an iteration count. The definition
   that follows the list is either a symbol to store in all the
   characters, or a function to call given the character. What the
   function returns is stored. extended-command is an alist of
   additional extended command names vs. definitions.

   If comtab a symbol, a new comtab is created with that name, with
   all lower-case characters indirected to the upper-case characters,
   and then the specified definitions are stored in it. In any case,
   the comtab is returned."
  (cond ((symbolp comtab)
	 (setq comtab (create-comtab comtab))
	 (set-comtab-uppercase-indirection comtab)))
  (do ((l specs (cddr l)))
      ((null l)
       nil)
    (cond ((consp (car l))
	   (do ((char (command-char-from-name (int-char (caar l)))
		      ;; increment to the next character
		      (int-char (1+ (char-int char))))
		(i 0 (1+ i))
		(to (cadar l))
		(command (cadr l)))
	       ((>= i to))
	     (command-store (if (symbolp command)
				command
				(funcall command char))
			    char
			    comtab)))
	  (t
	   (command-store (cadr l)
			  (command-char-from-name (int-char (car l)))
			  comtab))))
  (setf (comtab-extended-commands comtab)
	(append extended-commands (comtab-extended-commands comtab)))
  comtab)


;;; 1056


(defun barf (&optional ctl-string &rest args)
  "Report an error to the editor user.  Throws to the command loop.
   The args are a format string and args for format.  Signals the
   condition zwei:barf, passing along the args to barf."
  ;; There might not be a window, if preloading
  (let (tem1 tem2)
    (cond ((and (variable-boundp *window*) *window*)
	   ;; May have altered the text before erring
	   (must-redisplay *window* dis-text)
	   (if (eh:condition-name-handled-p 'barf)
	       (multiple-value-setq (tem1 tem2)
		 (apply #'signal 'barf ctl-string args)))
	   (if tem1
	       tem2
	       (progn
		 (and (send *standard-input* :send-if-handles :macro-error)
		      (format *query-io* "~&Keyboard macro terminated by error."))
		 (beep () *query-io*)
		 (when ctl-string
		   (fresh-line *query-io*)
		   (apply 'global:format *query-io* ctl-string args))
		 (throw 'zwei-command-loop t))))
	  (t (apply 'ferror 'barf-error ctl-string args)))))
