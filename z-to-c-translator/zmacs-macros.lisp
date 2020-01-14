



(in-package :zwei)

(defun str (&rest rest)
  (apply #'concatenate 'string rest))

(defvar *command-alist* ()
  "Alist of all ZWEI command names (strings) versus the commands themselves
   (symbols).")

(defun command-define (command doc ignore)
  (declare (ignore ignore))
  (cond ((stringp doc)
	 (setf (documentation command 'function) doc))
	((or (symbolp doc)
	     (and (not (atom doc))
		  (member (car doc) '(function lambda) :test #'eq)))
	 (setf (get command 'documentation-function) doc))
	(t
	 (ferror nil "The command ~s has invalid self-documentation ~s"
		 command doc)))
  (let* ((name (make-command-name command))
	 (aentry (assoc name *command-alist* :test 'equalp)))
    (setf (get command 'command-name) name)
    (if aentry
	(unless (eq command (cdr aentry))
	  (when (fquery nil (str "the command name ~s is currently set up "
				 "to call ~s; redefine it to call ~s? ")
				 name (cdr aentry) command)
	    (setf (cdr aentry) command)))
	(push (cons name command) *command-alist*))))


;;; Defines a command.  Form is:
;;; (DEFCOM COM-foo "Documentation." OPTIONS-LIST . BODY)
;;; Note: unlike EINE, there is no lambda-list.
(defmacro defcom (fn doc options &body def)
  "options are:(km)   -- this command always preserves mark.
               (sm)   -- set mark. there will be a region after this command
               (nm)   -- reset mark. there will be no region after this command
               (push) -- push point onto the point-pdl
               (r)    -- frob recentering fraction for forwa2rd move
               (-r)   -- frob recentering fraction for backward move."
  `(progn
     'compile
     (command-define ',fn ',doc ',options)
     (defun ,fn ()
       ,@(process-command-options options)
       ,@def)))

;; (cl-ppcre::all-matches "a" "foo bar baz")

(defparameter *last-match-positions* '())
(defparameter *last-match-strings* nil)

(defun string-match (pattern string &optional nth-match)
  (let* ((last-match-positions
	  (collecting
	    (cl-ppcre:do-matches (s e (cl-ppcre:parse-string pattern) string)
	      (collect (list s e)))))
	 (last-match-positions (if nth-match (subseq last-match-positions
						     nth-match)
				   last-match-positions))
	 (last-match-strings
	  (mapcar (lambda (position)
		    (apply #'subseq string position))
		  last-match-positions)))
    (setq *last-match-positions* last-match-positions)
    (setq *last-match-strings* last-match-strings)
  *last-match-positions*))

  ;; 	(collecting
  ;; 	  (cl-ppcre:do-matches (s e (cl-ppcre:parse-string pattern) string)
  ;; 	    (collect (list s e)))))
  ;; (car (nth nth-match *last-match-positions*)))

(defun match-beginning (nth-match)
  (car (nth nth-match *last-matches*)))

(defun match-end (nth-match)
  (cadr (nth nth-match *last-matches*)))

(defun substring (string start length)
  (subseq string start (+ start length)))

;; Under emacs rules second option should be nil if the last seach was
;; a buffer, or if it was a string, the same string that was
;; used. Since we don't have buffers, only the second condition
;; follows, so we always have the last string, and therefore never
;; need to re pass it, thus the second paramteter is ignorable.
(defun match-string (nth-match &optional ignore)
  (declare (ignore ignore ))
  (nth nth-match *last-match-strings*))

(defun parse-keys (string &optional need-vector)
  (let ((case-fold-search nil)
	;; We won't alter string in the loop below.
	(len (length string))
	(pos 0)
	(res '()))
    (loop while (and (< pos len)
		     (string-match
		      ;; "[^ \\t\\n\\f]+"
		      (:greedy-repetition
		       1 nil
		       (:inverted-char-class #\space #\tab #\newline #\page))
		      string pos))
      (let* ((word-beg (match-beginning 0))
	     (word-end (match-end 0))
	     (word (substring string word-beg len))
	     (times 1)
	     key)
	;; Try to catch events of the form "<as df>".
	(if (string-match
	     ;; "<[^ <>\\t\\n\\f][^>\\t\\n\\f]*>"
	     (:sequence
	      #\<
	      (:inverted-char-class #\space #\< #\> #\tab
				    #\newline #\page)
	      (:greedy-repetition
	       0 nil
	       (:inverted-char-class #\> #\tab #\newline #\page))
	      #\>)
	     word)
	    (setq word (match-string 0 word)
		  pos (+ word-beg (match-end 0)))
	  (setq word (substring string word-beg word-end)
		pos word-end))
	(when (string-match
	       ;; "([0-9]+)."
	       (:sequence
		(:register (:greedy-repetition
			    1 nil (:char-class (:range #\0 #\9))))
		:everything)
	       word)
	  (setq times (string-to-number (substring word 0 (match-end 1))))
	  (setq word (substring word (1+ (match-end 1)))))
	(cond ((string-match
		;; "^<<.+>>$"
		;; Execute extended command key irespective of where
		;; it's bound.
		(:sequence :start-anchor "<<"
			   (:greedy-repetition 1 nil :everything)
			   ">>" :end-anchor)
		word)
	       (setq key `((meta #\x)
			   ,(intern (string-upcase (substring word 2 -2))))))
	      ((and (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$" word)
		    (progn
		      (setq word (concat (substring word (match-beginning 1)
						    (match-end 1))
					 (substring word (match-beginning 3)
						    (match-end 3))))
		      (not (string-match
			    "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
			    word))))
	       (setq key (list (intern word))))
	      ((or (equal word "REM") (string-match "^;;" word))
	       (setq pos (string-match "$" string pos)))
	      (t
	       (let ((orig-word word) (prefix 0) (bits 0))
		 (while (string-match "^[ACHMsS]-." word)
		   (cl-incf bits (cdr (assq (aref word 0)
					 '((?A . ?\A-\^@) (?C . ?\C-\^@)
					   (?H . ?\H-\^@) (?M . ?\M-\^@)
					   (?s . ?\s-\^@) (?S . ?\S-\^@)))))
		   (cl-incf prefix 2)
		   (cl-callf substring word 2))
		 (when (string-match "^\\^.$" word)
		   (cl-incf bits ?\C-\^@)
		   (cl-incf prefix)
		   (cl-callf substring word 1))
		 (let ((found (assoc word '(("NUL" . "\0") ("RET" . "\r")
					    ("LFD" . "\n") ("TAB" . "\t")
					    ("ESC" . "\e") ("SPC" . " ")
					    ("DEL" . "\177")))))
		   (when found (setq word (cdr found))))
		 (when (string-match "^\\\\[0-7]+$" word)
		   (cl-loop for ch across word
                            for n = 0 then (+ (* n 8) ch -48)
                            finally do (setq word (vector n))))
		 (cond ((= bits 0)
			(setq key word))
		       ((and (= bits ?\M-\^@) (stringp word)
			     (string-match "^-?[0-9]+$" word))
			(setq key (cl-loop for x across word
                                           collect (+ x bits))))
		       ((/= (length word) 1)
			(error "%s must prefix a single character, not %s"
			       (substring orig-word 0 prefix) word))
		       ((and (/= (logand bits ?\C-\^@) 0) (stringp word)
			     ;; We used to accept . and ? here,
			     ;; but . is simply wrong,
			     ;; and C-? is not used (we use DEL instead).
			     (string-match "[@-_a-z]" word))
			(setq key (list (+ bits (- ?\C-\^@)
					   (logand (aref word 0) 31)))))
		       (t
			(setq key (list (+ bits (aref word 0)))))))))
	(when key
	  (cl-loop repeat times do (cl-callf vconcat res key)))))
    (when (and (>= (length res) 4)
	       (eq (aref res 0) ?\C-x)
	       (eq (aref res 1) ?\()
	       (eq (aref res (- (length res) 2)) ?\C-x)
	       (eq (aref res (- (length res) 1)) ?\)))
      (setq res (cl-subseq res 2 -2)))
    (if (and (not need-vector)
	     (cl-loop for ch across res
                      always (and (characterp ch)
                                  (let ((ch2 (logand ch (lognot ?\M-\^@))))
                                    (and (>= ch2 0) (<= ch2 127))))))
	(concat (cl-loop for ch across res
                         collect (if (= (logand ch ?\M-\^@) 0)
                                     ch (+ ch 128))))
      res)))



(defclass keyboard-key ()
  (modifiers key))

(defun kbd (keys)
  "Convert KEYS to the internal Emacs key representation.
KEYS should be a string constant in the format used for
saving keyboard macros (see `edmacro-mode')."
  ;; Don't use a defalias, since the `pure' property is only true for
  ;; the calling convention of `kbd'.
  (read-kbd-macro keys))

(defun kbd (keyboard-key-designator)

;; Convert
(kbd "<f2>")
